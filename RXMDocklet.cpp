/*
	RXMDocklet.cpp - "X" Monitor Docklet [DLL] implementation(s)

	Copyright(c) 2009-2021, Robert Roessler
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice,
	this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice,
	this list of conditions and the following disclaimer in the documentation
	and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
	ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

#include "stdafx.h"

#include <atomic>
#include <mutex>
#include <thread>
#include <string>
#include <string_view>
#include <sstream>
#include <fstream>
#include <memory>
#include <set>
#include <map>
#include <unordered_map>
#include <vector>
#include <regex>
#include <format>

#include "hwisenssm2.h"
#include "sdk/DockletSDK.h"
#include "TlHelp32.h"
#include "resource.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

/*
	Helper template functions to derive [c]begin/[c]end iterators for C++
	"native" multidimensional arrays... ultimately to be used with the std
	library, e.g. copy(...).
*/

// (make sure we are "noexcept" to the maximum extent possible)
#define NOEXCEPT_RETURN(...) noexcept(noexcept(__VA_ARGS__)) { return (__VA_ARGS__); }

template <typename T>
requires std::is_array_v<T>
constexpr decltype(auto) decayed_begin(T&& c)
NOEXCEPT_RETURN(std::begin(std::forward<T>(c)))

template <typename T>
requires std::is_array_v<T>
constexpr decltype(auto) decayed_end(T&& c)
NOEXCEPT_RETURN(std::end(std::forward<T>(c)))

template <typename T, size_t N>
requires std::is_array_v<T>
constexpr decltype(auto) decayed_begin(T(&c)[N])
NOEXCEPT_RETURN(reinterpret_cast<typename std::remove_all_extents_t<T>*>(c))

template <typename T, size_t N>
requires std::is_array_v<T>
constexpr decltype(auto) decayed_end(T(&c)[N])
NOEXCEPT_RETURN(reinterpret_cast<typename std::remove_all_extents_t<T>*>(c + N))

/*
	Define "alias templates" so as to use c++14 "is_transparent" comparators.
*/

template<typename T, typename Cmp = std::less<>>
using set = std::set<T, Cmp>;

template<typename K, typename T, typename Cmp = std::less<>>
using map = std::map<K, T, Cmp>;

using std::vector;
using std::string;
using std::wstring;
using std::string_view;
using std::begin, std::end, std::cbegin, std::cend;
using std::make_unique, std::unique_ptr;

using namespace std::string_literals;

// (resolve ambiguity with D2D declaration)
using RectF = Gdiplus::RectF;
using Color = Gdiplus::Color;

// (change the following line to true for tracing with ::OutputDebugStringA())
constexpr auto trace_enabled = true;

/*
	Construct trace message prefix including current "state machine" indicators.
*/
static auto tracePre(char* b, size_t n)
{
	if constexpr (trace_enabled) {
		const auto r = std::format_to_n(b, n, "RxTRACE> ");
		return string_view(b, r.size);
	}
}

template<typename... ARGS>
static void trace(const ARGS&... args)
{
	if constexpr (trace_enabled) {
		string_view fmt{ "{}{}{}{}{}{}{}{}", (min(sizeof...(ARGS), 7) + 1) * 2 };
		char b[32];
		::OutputDebugStringA(std::format(fmt, tracePre(b, std::size(b)), args...).c_str());
	}
}

template<typename... ARGS>
static void trace_ex(string_view fmt, const ARGS&... args)
{
	if constexpr (trace_enabled) {
		char b[32];
		::OutputDebugStringA(std::format("{}"s.append(fmt), tracePre(b, std::size(b)), args...).c_str());
	}
}

/*
	The rxm namespace contains all primary and supporting logic for
	accessing and displaying the provider-specific shared memory based
	data representations of some common "hardware monitors"... the
	expected and supported client is the RXMDocklet plugin, compatible
	with the ObjectDock Docklet SDK v1.0 interface specification(*).

	Currently supported apps include GPU-Z, HWiNFO, CPUID HWMonitor,
	MSI Afterburner, and SpeedFan.

	* - this has only been tested/used with the [final] 1.3.5 version
	of RocketDock... and even though the RocketDock download page says
	that it is "unsupported" on 64-bit versions of Windows, it works!
*/
namespace rxm {

constexpr size_t sizeOfUTF8CodeUnits(int u)
{
	return
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 00-0f 1-byte UTF-8/ASCII
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 10-1f
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 20-2f
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 30-3f
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 40-4f
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 50-5f
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 60-6f
		"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 70-7f

		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// 80-8f <illegal>
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// 90-9f
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// a0-af
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// b0-bf

		"\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2"	// c0-cf 2-byte UTF-8
		"\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2"	// d0-df

		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"	// e0-ef 3-byte UTF-8

		"\4\4\4\4\4\4\4\4"					// f0-f7 4-byte UTF-8

		"\0\0\0\0\0\0\0\0"					// f8-ff <illegal>
		[u & 0xff];
}

constexpr size_t sizeOfUTF16CodeUnits(int u)
{
	return
		u < 0xd800 ? 1 :
//		u < 0xdc00 ? 2 :	// (no need to distinguish this range separately)
		u < 0xe000 ? 2 :
		1;
}

template<class CharOutput>
constexpr void codePointToUTF8(char32_t c, CharOutput f)
{
	if (c < 0x80)
		f((char)c);
	else if (c < 0x800)
		f((char)(0b11000000 | (c >> 6))),
		f((char)((c & 0b111111) | 0b10000000));
	else if (c < 0x10000)
		f((char)(0b11100000 | (c >> 12))),
		f((char)(((c >> 6) & 0b111111) | 0b10000000)),
		f((char)((c & 0b111111) | 0b10000000));
	else
		f((char)(0b11110000 | (c >> 18))),
		f((char)(((c >> 12) & 0b111111) | 0b10000000)),
		f((char)(((c >> 6) & 0b111111) | 0b10000000)),
		f((char)((c & 0b111111) | 0b10000000));
}

template<class WordOutput>
constexpr void codePointToUTF16(char32_t c, WordOutput g)
{
	if (c < 0xd800 || (c >= 0xe000 && c < 0x10000))
		g((wchar_t)c);
	else {
		const unsigned int v = c - 0x10000;
		g((wchar_t)(0xd800 | (v & 0x3ff))), g((wchar_t)(0xdc00 | (v >> 10)));
	}
}

constexpr char32_t codePointFromUTF8(const char* u) {
	const auto c = u[0];
	switch (sizeOfUTF8CodeUnits(c)) {
	case 1: return c;
	case 2: return (c & 0b11111) << 6 | (u[1] & 0b111111);
	case 3: return (c & 0b1111) << 12 | (u[1] & 0b111111) << 6 | (u[2] & 0b111111);
	case 4: return (c & 0b111) << 18 | (u[1] & 0b111111) << 12 | (u[2] & 0b111111) << 6 | (u[3] & 0b111111);
	}
	return 0; // ("can't happen")
}

constexpr char32_t codePointFromUTF16(const wchar_t* u)
{
	return
		sizeOfUTF16CodeUnits(u[0]) == 1 ? u[0] :
		((u[0] - 0xd800) << 10) + (u[1] - 0xdc00) + 0x10000;
}

constexpr string utf8StringFromUTF16(const wchar_t* u)
{
	string t;
	while (*u)
		codePointToUTF8(codePointFromUTF16(u), [&t](char c) { t.push_back(c); }),
			u += sizeOfUTF16CodeUnits(*u);
	return t;
}

constexpr wstring utf16StringFromUTF8(const char* u)
{
	wstring t;
	while (*u)
		codePointToUTF16(codePointFromUTF8(u), [&t](wchar_t c) { t.push_back(c); }),
			u += sizeOfUTF8CodeUnits(*u);
	return t;
}

/*
	Definition and implementation of simple [threading-aware] spinlock
*/
class RSpinLock {
	std::atomic_flag lock_{};

public:
	auto lock() {
		// try simple lock...
		while (lock_.test_and_set(std::memory_order_acquire))
			// ... nope, release time slice and keep trying
			std::this_thread::yield();
	}
	auto unlock() { lock_.clear(std::memory_order_release); }
};

/*
	Implementation of Windows-style shared memory mapping
*/
class Mapping {
	HANDLE mH{};						// mapped obj handle
	LPBYTE vB{};						// mapped obj view base
	size_t vN{};						// mapped obj view size

public:
	Mapping() {}
	~Mapping ()
	{
		if (vN != 0)
			::UnmapViewOfFile(vB), ::CloseHandle(mH);
	}

	auto Create(const char* sharedObjName)
	{
		if (vN != 0)
			return
				trace_ex("MAPPING of '{}' ALREADY PRESENT @{:p} with {:#x} bytes", sharedObjName, (void*)vB, vN),
					true;	// (mapping ALREADY here)

		mH = ::OpenFileMapping(GENERIC_READ, FALSE, sharedObjName);
		if (mH == nullptr)
			return false;	// we're outta here

		vB = (LPBYTE)::MapViewOfFile(mH, FILE_MAP_READ, 0, 0, 0);
		if (vB == nullptr) {
			::CloseHandle(mH), mH = nullptr;
			return false;	// we're outta here
		}

		MEMORY_BASIC_INFORMATION mbI;
		if (::VirtualQuery(vB, &mbI, sizeof mbI) != sizeof mbI) {
			::UnmapViewOfFile(vB), vB = nullptr, ::CloseHandle(mH), mH = nullptr;
			return false;	// we're outta here
		}

		vN = mbI.RegionSize;	// NOW it's a full mapping
		trace_ex("MAPPED '{}' @{:p} with {:#x} bytes", sharedObjName, (void*)vB, vN);
		return true;
	}

	constexpr auto Base() const { return vB; }
	constexpr auto Size() const { return vN; }
};

/*
	primary types for dealing with sensor paths and collections
*/
using sensor_t = string;
using sensor_enumeration_t = set<sensor_t>;

// (generic COMPILE-TIME "get integer value for typesafe enum" function)
template<class E>
requires std::is_enum_v<E>
constexpr auto as_int(E u) { return static_cast<std::underlying_type_t<E>>(u); }

enum class LayoutConf { Pages = 4, LayoutsPerPage = 8, BackgroundImages = 10 };

enum class RenderType { Normal = 0, Forced, StartFocus, EndFocus };

/*
	"universal" units to which all Monitor-specific units are mapped
*/
enum class Unit {
	None,
	// "base" units understood across most providers
	Volts, Degrees, RPM, Amps, Watts, MHz, UsagePerCent,
	// "extended" units from "comprehensive" providers
	MB, MBs, YorN, GTs, T, X, KBs,
	// even MORE "extended" units
	FPS, MS, GB,
	Unknown
};

/*
	utility functions for accessing [sensor] path components
*/

constexpr string_view head(string_view path)
{
	const auto i = path.find_first_of('|');
	return i != string::npos ? path.substr(0, i) : "";
}

constexpr string_view tail(string_view path)
{
	const auto i = path.find_last_of('|');
	return i != string::npos ? path.substr(i + 1) : "";
}

static inline vector<string> split(string_view path, const std::regex& sep)
{
	std::cregex_token_iterator first(path.data(), path.data() + path.size(), sep, -1), last;
	return { first, last };
}

/*
	primary INTERFACE for talking to [abstract] "Monitors"...
*/
class IMonitor {
public:
	virtual ~IMonitor() {};

	virtual constexpr string DisplayName() const = 0;
	virtual bool Refresh() = 0;
	virtual constexpr bool RefreshNeeded() const = 0;
	virtual const sensor_enumeration_t& Sensors() const = 0;
	virtual constexpr Unit SensorUnit(string_view path) const = 0;
	virtual constexpr string SensorUnitString(string_view path, bool fahrenheit = false) const = 0;
	virtual constexpr float SensorValue(string_view path, bool fahrenheit = false) const = 0;
	virtual constexpr string SensorValueString(string_view path, bool fahrenheit = false) const = 0;
};

/*
	... common functions and data elements supporting IMonitor IMPLEMENTATIONS
*/
template<typename T>
class MonitorCommonImpl : public IMonitor {
protected:
	using value_type = T;
	using enum Unit;

	Mapping mapping;
	string root, displayName;
	sensor_enumeration_t sensors;
	map<sensor_t, value_type> values;
	map<sensor_t, Unit> units;
	RSpinLock lock;

	MonitorCommonImpl(string root, string displayName) : root(root), displayName(displayName) {}

	constexpr auto c2f(double d) const { return floor((d * 9 / 5 + 32) + 0.5); }
	template<class SynchronizedInit>
	bool refreshImpl(SynchronizedInit f) {
		std::lock_guard acquire(lock);
		return f();
	}
	template<class RawValueAccess>
	float sensorValueImpl(string_view path, bool fahrenheit, RawValueAccess f) const {
		float ret = 0;
		std::lock_guard acquire(const_cast<RSpinLock&>(lock));
		const auto&& v = values.find(path);
		const auto&& u = units.find(path);
		if (v == cend(values) || u == cend(units))
			ret = std::numeric_limits<float>::infinity();	// sensor not present
		else try {
			double d = f(v->second, u->second);
			if (u->second == Degrees && fahrenheit)
				d = c2f(d);
			ret = (float)d;
		} catch (...) {
			ret = std::numeric_limits<float>::infinity();	// sensor not present
		}
		return ret;
	}

public:
	constexpr string DisplayName() const override { return displayName; }
	constexpr bool RefreshNeeded() const override { return false; }
	const sensor_enumeration_t& Sensors() const override { return sensors; }
	constexpr Unit SensorUnit(string_view path) const override {
		const auto&& u = units.find(path);
		return u != cend(units) ? u->second : None;
	}
	constexpr string SensorUnitString(string_view path, bool fahrenheit) const override {
		const auto u = SensorUnit(path);
		/*
			display representation for above "universal" units

			N.B. - Unit enums will be used as indices into this array, so
			make SURE they are kept in sync!
		*/
		static const constinit char* unitString[]{
			"None",
			"V", (const char*)u8"°", "rpm", "A", "W", "MHz", "%",
			"MB", "MB/s", "", "GT/s", "T", "x", "KB/s",
			"F/s", "ms", "GB",
			"???"
		};
		string s{ unitString[as_int(u)] };
		if (u == Degrees)
			s.push_back(fahrenheit ? 'F' : 'C');
		return s;
	}
	constexpr string SensorValueString(string_view path, bool fahrenheit) const override {
		const auto u = SensorUnit(path);
		const auto v = SensorValue(path, fahrenheit);
		/*
			# of fractional digits to display for above "universal" units

			N.B. - Unit enums will be used as indices into this array, so
			make SURE they are kept in sync!
		*/
		auto w{
			"\000"
			"\003\000\000\003\003\001\001"
			"\000\003\000\001\000\000\003"
			"\000\000\000"
			"\000"
			[as_int(u)]};
		/*
			Use "dynamic precision reduction" to stay within ~4 digits...
			"fractional digits" width value is a *hint*, not absolute!
		*/
		if (w == 1) {
			// try dropping decimal on simple width check...
			if (v >= 1000 ||
				// ... or try "Fan" heuristic (aka "hack")
				(u == UsagePerCent && path.find("Fan") != string::npos))
				w = 0;
		} else if (w == 3)
			if (v >= 100)
				w = 1;
			else if (v >= 10)
				w = 2;
		return std::format("{:.{}f}", v, w);
	}
};

/*
	Implementation of IMonitor for MSI Afterburner
*/
class ABMonitor : public MonitorCommonImpl<const float*> {
	using enum Unit;

#pragma pack(push, 1)
	struct MAHM_SHARED_MEMORY_HEADER {
		DWORD dwSignature;				// 'MAHM' or not currently valid
		DWORD dwVersion;				// version of structure 2.0 == 0x20000
		DWORD dwHeaderSize;				// our size
		DWORD dwNumEntries;				// # of MAHM_SHARED_MEMORY_ENTRYs
		DWORD dwEntrySize;				// size of MAHM_SHARED_MEMORY_ENTRY
		// WARNING! Force 32-bit time_t usage with #define _USE_32BIT_TIME_T 
		// to provide compatibility with VC8.0 and newer compiler versions,
		// OR force use of __time32_t directly!
		__time32_t time;				// last poll time
		DWORD dwNumGpuEntries;			// # of MAHM_SHARED_MEMORY_GPU_ENTRYs
		DWORD dwGpuEntrySize;			// size of MAHM_SHARED_MEMORY_GPU_ENTRY
	};

	enum ShowTarget { ShowInOSD = 1, ShowInLCD = 2, ShowInTray = 4 };

	struct MAHM_SHARED_MEMORY_ENTRY {
		char szSrcName[MAX_PATH];		// source name
		char szSrcUnits[MAX_PATH];		// source units
		char szLocSrcName[MAX_PATH];	// [localized] source name
		char szLocSrcUnits[MAX_PATH];	// [localized] source units
		char szFormat[MAX_PATH];		// preferred formatting string
		float data;						// last value or FLOAT_MAX
		float minLimit;
		float maxLimit;
		DWORD dwFlags;					// control flags (from ShowTarget enum)
		DWORD dwGpu;					// GPU # component of data src
		DWORD dwSrcId;					// [per-GPU] ID # component of data src
	};

	struct MAHM_SHARED_MEMORY_GPU_ENTRY {
		char szGpuId[MAX_PATH];			// Device Manager -style path name
		char szFamily[MAX_PATH];		// GPU "family"
		char szDevice[MAX_PATH];		// device description
		char szDriver[MAX_PATH];		// driver descriptive name
		char szBIOS[MAX_PATH];			// BIOS descriptive name
		DWORD dwMemAmount;				// device memory in KB
	};
#pragma pack(pop)

	int enumerateSensors();
	auto& ab() const { return *(const MAHM_SHARED_MEMORY_HEADER*)mapping.Base(); }
	auto& rE(int i) const { return *(MAHM_SHARED_MEMORY_ENTRY*)(mapping.Base() + ab().dwHeaderSize + ab().dwEntrySize * i); }
	auto& gE(int i) const { return *(MAHM_SHARED_MEMORY_GPU_ENTRY*)(mapping.Base() + ab().dwHeaderSize + ab().dwEntrySize * ab().dwNumEntries + ab().dwGpuEntrySize * i); }
	auto unitFromRecord(const MAHM_SHARED_MEMORY_ENTRY& r) const;

public:
	ABMonitor(string root, string displayName) : MonitorCommonImpl(root, displayName) {}
	~ABMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create("MAHMSharedMemory") && enumerateSensors() > 0; });
	}
	float SensorValue(string_view path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) { return *i; });
	}
};

auto ABMonitor::unitFromRecord(const MAHM_SHARED_MEMORY_ENTRY& r) const
{
	// N.B. - MSI Afterburner uses (at best) a narrow code-page value for <degrees>
	static const map<string, Unit> types{
		{ "V", Volts }, { "\xb0""C", Degrees }, { "RPM", RPM }, { "MHz", MHz },
		{ "%", UsagePerCent }, { "MB", MB }, { "FPS", FPS }, { "ms", MS },
		{"W", Watts}
	};
	const auto&& u = types.find(r.szSrcUnits);
	return u != cend(types) ? u->second : Unknown;
}

int ABMonitor::enumerateSensors()
{
	::OutputDebugString("ABMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	const auto& a = ab();

	if (a.dwSignature != 'MAHM')
		return 0; // nothing to see here...

	for (decltype(a.dwNumEntries) i = 0; i < a.dwNumEntries; ++i) {
		const auto& r = rE(i);
		if (const auto u = unitFromRecord(r); u != None) {
			auto path{ std::format("{}|", root) };
			if (r.dwSrcId != 0xffffffff && r.dwSrcId < 0x80) {
				std::format_to(std::back_inserter(path), "{}", gE(r.dwGpu).szDevice);
				/*
					append #2, #3 etc for any subsequent GPUs - a simplifying
					assumption, since cases when they AREN'T in a "standard"
					SLI/Crossfire configuration should be rare (we will see
					how the case of discrete GPU + integrated GPU stabilizes)
				*/
				if (r.dwGpu > 0)
					std::format_to(std::back_inserter(path), " #{}", r.dwGpu + 1);
			} else
				path += "pc";
			std::format_to(std::back_inserter(path), "|{}", r.szSrcName);
			sensors.insert(path);
			values[path] = &r.data, units[path] = u;
			::OutputDebugString(path.c_str());
		}
	}

	trace("ABMonitor found ", sensors.size(), " sensors");
	return sensors.size();
}

/*
	Implementation of IMonitor for Core Temp
*/
class CTMonitor : public MonitorCommonImpl<const float*> {
	using enum Unit;

#pragma pack(push, 1)
	typedef struct core_temp_shared_data_ex {
		// Original structure (CoreTempSharedData)
		unsigned int	uiLoad[256];
		unsigned int	uiTjMax[128];
		unsigned int	uiCoreCnt;
		unsigned int	uiCPUCnt;
		float			fTemp[256];
		float			fVID;
		float			fCPUSpeed;
		float			fFSBSpeed;
		float			fMultiplier;
		char			sCPUName[100];
		unsigned char	ucFahrenheit;
		unsigned char	ucDeltaToTjMax;
		// uiStructVersion = 2
		unsigned char	ucTdpSupported;
		unsigned char	ucPowerSupported;
		unsigned int	uiStructVersion;
		unsigned int	uiTdp[128];
		float			fPower[128];
		float			fMultipliers[256];
	} CoreTempSharedDataEx, *LPCoreTempSharedDataEx, **PPCoreTempSharedDataEx;
#pragma pack(pop)

	int enumerateSensors();
	auto& ct() const { return *(const CoreTempSharedDataEx*)mapping.Base(); }

public:
	CTMonitor(string root, string displayName) : MonitorCommonImpl(root, displayName) {}
	~CTMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create("CoreTempMappingObjectEx") && enumerateSensors() > 0; });
	}
	float SensorValue(string_view path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [this](auto i, Unit u) {
			const auto& c = ct();
			return u == Degrees ?
				(c.ucDeltaToTjMax ? c.uiTjMax[0] - *i : *i) :
				*(const unsigned int*)i;
		});
	}
};

int CTMonitor::enumerateSensors()
{
	::OutputDebugString("CTMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	const auto& c = ct();

	if (c.uiStructVersion != 2)
		return 0; // nothing to see here...

	for (decltype(c.uiCPUCnt) cpu = 0; cpu < c.uiCPUCnt; ++cpu)
		for (decltype(c.uiCoreCnt) core = 0; core < c.uiCoreCnt; ++core) {
			auto off = [c](auto i, auto j) { return c.uiCoreCnt * i + j; };
			const auto corePath{ std::format("{}|CPU [#{}]: {}|Core #{}", root, cpu, c.sCPUName, core) };
			const auto tempPath{ corePath + " Temperature" };
			sensors.insert(tempPath), values[tempPath] = c.fTemp + off(cpu, core), units[tempPath] = Degrees;
			::OutputDebugString(tempPath.c_str());
			const auto loadPath{ corePath + " Load" };
			sensors.insert(loadPath), values[loadPath] = (value_type)(c.uiLoad + off(cpu, core)), units[loadPath] = UsagePerCent;
			::OutputDebugString(loadPath.c_str());
		}

	trace("CTMonitor found ", sensors.size(), " sensors");
	return sensors.size();
}

/*
	Implementation of IMonitor for GPU-Z
*/
class GPUZMonitor : public MonitorCommonImpl<const double*> {
	using enum Unit;
	enum { MaxRecords = 128 };

#pragma pack(push, 1)
	struct Record {
		wchar_t key[256];
		wchar_t val[256];
	};

	struct SensorRecord {
		wchar_t name[256];
		wchar_t unit[8];
		DWORD digits;
		double value;
	};

	struct GpuzSharedMem {
		DWORD version;					// version of structure
		volatile LONG busy;				// updatING
		DWORD updated;					// [last] updatED from ::GetTickCount()
		Record data[MaxRecords];
		SensorRecord sensors[MaxRecords];
	};
#pragma pack(pop)

	int enumerateSensors();
	auto& gpuz() const { return *(const GpuzSharedMem*)mapping.Base(); }
	auto unitFromRecord(const SensorRecord& r) const;

public:
	GPUZMonitor(string root, string displayName) : MonitorCommonImpl(root, displayName) {}
	~GPUZMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create("GPUZShMem") && enumerateSensors() > 0; });
	}
	float SensorValue(string_view path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) { return *i; });
	}
};

auto GPUZMonitor::unitFromRecord(const SensorRecord& r) const
{
	static const map<wstring, Unit> types{
		{L"V", Volts}, {L"°C", Degrees}, {L"RPM", RPM}, {L"MHz", MHz},
		{L"%", UsagePerCent}, {L"%%", UsagePerCent}
	};
	const auto&& u = types.find(r.unit);
	return u != cend(types) ? u->second : None;
}

int GPUZMonitor::enumerateSensors()
{
	::OutputDebugString("GPUZMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	const auto& g = gpuz();

	const auto&& device = std::find_if(cbegin(g.data), cend(g.data), [](const auto& r) {
		return strcmp(utf8StringFromUTF16(r.key).c_str(), "CardName") == 0;
	});
	string deviceName{ utf8StringFromUTF16(device != cend(g.data) ? device->val : L"<Graphics Card>") };

	for (const auto& s : g.sensors) {
		if (s.name[0] == L'\0')
			break;	// nothing more to examine
		if (const auto u = unitFromRecord(s); u != None) {
			const auto path{ std::format("{}|{}|{}", root, deviceName, utf8StringFromUTF16(s.name)) };
			sensors.insert(path);
			values[path] = &s.value, units[path] = u;
			::OutputDebugString(path.c_str());
		}
	}

	trace("GPUZMonitor found ", sensors.size(), " sensors");
	return sensors.size();
}

/*
	Implementation of IMonitor for HWiNFO
*/
class HWiMonitor : public MonitorCommonImpl<DWORD> {
	using enum Unit;

	int enumerateSensors();
	auto& hwi() const { return *(const HWiNFO_SENSORS_SHARED_MEM2*)mapping.Base(); }
	auto& sE(int i) const { return *(PHWiNFO_SENSORS_SENSOR_ELEMENT)(mapping.Base() + hwi().dwOffsetOfSensorSection + hwi().dwSizeOfSensorElement * i); }
	auto& rE(int i) const { return *(PHWiNFO_SENSORS_READING_ELEMENT)(mapping.Base() + hwi().dwOffsetOfReadingSection + hwi().dwSizeOfReadingElement * i); }
	auto unitFromReading(const HWiNFO_SENSORS_READING_ELEMENT& r) const;

	DWORD origSensors = 0;
	DWORD origReadings = 0;

public:
	HWiMonitor(string root, string displayName) : MonitorCommonImpl(root, displayName) {}
	~HWiMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create("" HWiNFO_SENSORS_MAP_FILE_NAME2) && enumerateSensors() > 0; });
	}
	bool RefreshNeeded() const override {
		const auto& h = hwi();
		return h.dwNumSensorElements != origSensors || h.dwNumReadingElements != origReadings;
	}
	float SensorValue(string_view path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [this](auto i, Unit u) { return rE(i).Value; });
	}
	string SensorValueString(string_view path, bool fahrenheit = false) const override {
		if (SensorUnit(path) == YorN)
			return SensorValue(path) != 0 ? "Yes" : "No";
		else
			return MonitorCommonImpl::SensorValueString(path, fahrenheit);
	}
};

auto HWiMonitor::unitFromReading(const HWiNFO_SENSORS_READING_ELEMENT& r) const
{
	switch (r.tReading) {
	case SENSOR_TYPE_NONE:
		return None;
	case SENSOR_TYPE_TEMP:
		return Degrees;
	case SENSOR_TYPE_VOLT:
		return Volts;
	case SENSOR_TYPE_FAN:
		return RPM;
	case SENSOR_TYPE_CURRENT:
		return Amps;
	case SENSOR_TYPE_POWER:
		return Watts;
	case SENSOR_TYPE_CLOCK:
		return MHz;
	case SENSOR_TYPE_USAGE:
		return UsagePerCent;
	case SENSOR_TYPE_OTHER: {
		// try to deduce our Unit from the "units" string in the READING...
		// [UsagePerCent], MB, MBs, YorN, GTs, T, X, KBs
		static const map<string, Unit> extendedTypes{
			{"%", UsagePerCent},
			{"MB", MB}, {"MB/s", MBs}, {"Yes/No", YorN}, {"GT/s", GTs},
			{"T", T}, {"x", X}, {"KB/s", KBs}, {"GB", GB}
		};
		const auto&& u = extendedTypes.find(r.szUnit);
		return u != cend(extendedTypes) ? u->second : Unknown; // we did our best
	}
	default:
		return None; // "shouldn't happen"
	}
}

int HWiMonitor::enumerateSensors()
{
	::OutputDebugString("HWiMonitor::enumerateSensors...");
	// create a "sensorNameFromInstanceNumber"
	auto computedSensorName = [](auto s) {
		string deviceName{ s.szSensorNameUser };
		if (s.dwSensorInst > 0)
			std::format_to(std::back_inserter(deviceName), " #{}", s.dwSensorInst + 1);
		return deviceName;
	};
	sensors.clear(), values.clear(), units.clear();
	const auto& h = hwi();

	if (h.dwSignature != 'SiWH')
		return 0; // nothing to see here...

	origSensors = h.dwNumSensorElements, origReadings = h.dwNumReadingElements;
	for (decltype(h.dwNumReadingElements) i = 0; i < h.dwNumReadingElements; ++i) {
		const auto& r = rE(i);
		if (const auto u = unitFromReading(r); u != None) {
			const auto& s = sE(r.dwSensorIndex);
			auto path{ std::format("{}|{}|{}", root, computedSensorName(s), r.szLabelUser) };
			if (r.szUnit[0])
				std::format_to(std::back_inserter(path), " {}", r.szUnit);
			sensors.insert(path);
			values[path] = i, units[path] = u;
			::OutputDebugString(path.c_str());
		}
	}

	trace("HWiMonitor found ", sensors.size(), " sensors");
	return sensors.size();
}

/*
	Implementation of IMonitor for CPUID HWMonitor

	N.B. - ONLY good for versions 1.14 - 1.16 of HWMonitor - after that, they
	appear to have "locked down" the shared memory interface such that only
	zeros are returned.
*/
class HWMonitor : public MonitorCommonImpl<const float*> {
	enum { MaxGroups = 10 };				// HW Monitor 1.14-1.16 (1.13 was 9)

	typedef struct {
		unsigned char pad1[80];				// unknown
		DWORD deviceNum;					// # of HWMDevice structs
											// HWMDevice structs base
	} HWMHdr;

	typedef struct {
		DWORD nodeNum;						// # of HWMSensor structs
		DWORD nodePtr;						// HWMSensor structs addr
	} HWMSensorMap;

	typedef struct {
		char description[64];				// full descriptive device name
		DWORD type;							// type bit mask???
		DWORD layout;						// type extension / groups layout???
		HWMSensorMap map[MaxGroups];		// map of HWMSensor structs
	} HWMDevice;

	typedef struct {
		unsigned char pad1[8];				// unknown
		char name[32];						// terse sensor name
		DWORD value0;						// sensor val (raw?)
		float value;						// sensor val
	} HWMSensor;

	const auto& device(int d) const { return ((const HWMDevice*)(mapping.Base() + sizeof HWMHdr))[d]; }
	int deviceCount() const { return mapping.Base() ? ((const HWMHdr*)mapping.Base())->deviceNum : 0; }
	const char* deviceDescription(int d) const { return mapping.Base() && d < deviceCount() ? device(d).description : "" ; }
	int enumerateSensors();
	const char* groupType(int g) const {
		static const constinit char* sensorGroupTypes[MaxGroups]{
			"<voltages>",
			"<temperatures>",
			"<fans>",
			"<PWM fans>",
			"<currents>",
			"<powers>",
			"xxx", "xxx", "xxx", "xxx"
		};
		return g < MaxGroups ? sensorGroupTypes[g] : "";
	}
	const auto& node(int d, int g, int s) const { return ((const HWMSensor*)(mapping.Base() + device(d).map[g].nodePtr))[s]; }
	int sensorCount(int d, int g) const { return mapping.Base() && d < deviceCount() && g < MaxGroups ? device(d).map[g].nodeNum : 0; }
	const char* sensorLabel(int d, int g, int s) const { return mapping.Base() && d < deviceCount() && g < MaxGroups && s < sensorCount(d, g) ? node(d, g, s).name : ""; }
	auto unitFromDGS(int d, int g, int s) const;

public:
	HWMonitor(string root, string displayName) : MonitorCommonImpl(root, displayName) {}
	~HWMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create("$CPUID$HWM$") && enumerateSensors() > 0; });
	}
	float SensorValue(string_view path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) { return *i; });
	}
};

auto HWMonitor::unitFromDGS(int d, int g, int s) const {
	using enum Unit;
	static constexpr Unit g2u[]{ Volts, Degrees, RPM, RPM, Amps, Watts };
	return mapping.Base() && d < deviceCount() && g < MaxGroups&& s < sensorCount(d, g) ? g2u[g] : None;
}

int HWMonitor::enumerateSensors()
{
	auto dgs = [](auto d, auto g, auto s) {
		return std::format("{},{},{}", d, g, s);
	};
	::OutputDebugString("HWMonitor::enumerateSensors...");
	std::multiset<string> devices;
	sensors.clear(), values.clear(), units.clear();

	for (auto d = 0; d < deviceCount(); ++d) {
		const string deviceName{ deviceDescription(d) };
		devices.insert(deviceName);
		const auto dupes = devices.count(deviceName);
		for (auto g = 0; g < MaxGroups; ++g)
			for (auto s = 0; s < sensorCount(d, g); ++s) {
				auto path{ std::format("{}|{}", root, deviceName) };
				if (dupes > 1)
					std::format_to(std::back_inserter(path), " #{}", dupes);
				// N.B. - ONLY CPUID Hardware Monitor needs this "extra" level
				std::format_to(std::back_inserter(path), "|{}|{}", groupType(g), sensorLabel(d, g, s));
				sensors.insert(path);
				values[path] = &node(d, g, s).value;
				units[path] = unitFromDGS(d, g, s);
				::OutputDebugString((dgs(d, g, s) + '=' + path).c_str());
			}
	}

	trace("HWMonitor found ", sensors.size(), " sensors");
	return sensors.size();
}

/*
	Implementation of IMonitor for SpeedFan

	N.B. - a number of the IT8720F sensor readings for voltages appear to be
	incorrect - as these show this way in the SpeedFan app itself, they are
	of course passed on by RXMDocklet... YMMV, of course.
*/
class SFMonitor : public MonitorCommonImpl<const int*> {
	enum { Temps = 32, Fans = 32, Voltages = 32 };
	enum ParseState {
		WantVer, WantTag, WantDeviceName, WantTempName, WantFanName, WantVoltName, WantEnd
	};

#pragma pack(push, 1)
	struct SfSharedMem {
		WORD version;					// version of structure
		WORD flags;						// flags(?)
		size_t size;					// size of structure
		HANDLE handle;					// mapping handle
		WORD nTemperatures;				// # of valid temperature values
		WORD nFans;						// # of valid fan values
		WORD nVoltages;					// # of valid voltage values
		int temps[Temps];				// actual temperature values
		int fans[Fans];					// actual fan values
		int volts[Voltages];			// actual voltage values
	};
#pragma pack(pop)

	const char* speedFanConfig = "speedfansens.cfg";
	const char* speedFanExecutable = "speedfan.exe";
	
	const char* cfgVersionTag = "xxx version ";
	const int cfgVersionTagN = strlen(cfgVersionTag);
	const char* cfgLinkTag = "xxx Link UniqueID=";
	const int cfgLinkTagN = strlen(cfgLinkTag);
	const char* cfgPwmTag = "xxx Pwm ";
	const int cfgPwmTagN = strlen(cfgPwmTag);
	const char* cfgSensorTag = "xxx Sensor UniqueID=";
	const int cfgSensorTagN = strlen(cfgSensorTag);
	const char* cfgDeviceNameTag = "Name=";
	const int cfgDeviceNameTagN = strlen(cfgDeviceNameTag);
	const char* cfgSensorNameTag = "name=";
	const int cfgSensorNameTagN = strlen(cfgSensorNameTag);
	const char* cfgTempTag = "xxx Temp ";
	const int cfgTempTagN = strlen(cfgTempTag);
	const char* cfgFanTag = "xxx Fan ";
	const int cfgFanTagN = strlen(cfgFanTag);
	const char* cfgVoltTag = "xxx Volt ";
	const int cfgVoltTagN = strlen(cfgVoltTag);
	const char* cfgEndTag = "xxx end";
	const int cfgEndTagN = strlen(cfgEndTag);

	int enumerateSensors();
	string getExecutableDir(string_view exeToFind);
	auto& sf() const { return *(const SfSharedMem*)mapping.Base(); }

public:
	SFMonitor(string root, string displayName) : MonitorCommonImpl(root, displayName) {}
	~SFMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create("SFSharedMemory_ALM") && enumerateSensors() > 0; });
	}
	float SensorValue(string_view path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) {
			return u != RPM ? (double)*i / 100 : (double)*i;
		});
	}
};

int SFMonitor::enumerateSensors()
{
	::OutputDebugString("SFMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	const string sfPath = getExecutableDir(speedFanExecutable);
	if (sfPath.empty())
		return 0;	// we're outta here
	std::ifstream config(sfPath + speedFanConfig);
	if (!config.good())
		return 0;	// we're outta here

	string version, longName;
	map<string, string> devices;
	int temps = 0, fans = 0, volts = 0;
	ParseState state = WantVer;
	char line[132];
	while (config.getline(line, 132).good()) {
		if (line[0] == '\0')
			continue;

		switch (state) {
		using enum Unit;
		case WantVer:
			if (strncmp(line, cfgVersionTag, cfgVersionTagN) == 0)
				version = line + cfgVersionTagN, state = WantTag;
			break;

		case WantTag:
			if (strncmp(line, cfgLinkTag, cfgLinkTagN) == 0 ||
				strncmp(line, cfgPwmTag, cfgPwmTagN) == 0)
				state = WantEnd;	// (ignore these and skip to end)
			else if (strncmp(line, cfgSensorTag, cfgSensorTagN) == 0)
				longName = line + cfgSensorTagN, state = WantDeviceName;
			else if (strncmp(line, cfgTempTag, cfgTempTagN) == 0)
				longName = strstr(line, " from ") + 6, state = WantTempName;
			else if (strncmp(line, cfgFanTag, cfgFanTagN) == 0)
				longName = strstr(line, " from ") + 6, state = WantFanName;
			else if (strncmp(line, cfgVoltTag, cfgVoltTagN) == 0)
				longName = strstr(line, " from ") + 6, state = WantVoltName;
			break;

		case WantDeviceName:
			if (strncmp(line, cfgDeviceNameTag, cfgDeviceNameTagN) == 0)
				devices[longName] = line + cfgDeviceNameTagN, state = WantEnd;
			break;

		case WantTempName:
			if (strncmp(line, cfgSensorNameTag, cfgSensorNameTagN) == 0) {
				const auto path{ std::format("{}|{}|<temperatures>|{}", root, devices[longName], line + cfgSensorNameTagN) };
				values[path] = &sf().temps[temps++], units[path] = Degrees;
				sensors.insert(path);
				::OutputDebugString(path.c_str());
				state = WantEnd;
			}
			break;

		case WantFanName:
			if (strncmp(line, cfgSensorNameTag, cfgSensorNameTagN) == 0) {
				const auto path{ std::format("{}|{}|<fans>|{}", root, devices[longName], line + cfgSensorNameTagN) };
				values[path] = &sf().fans[fans++], units[path] = RPM;
				sensors.insert(path);
				::OutputDebugString(path.c_str());
				state = WantEnd;
			}
			break;

		case WantVoltName:
			if (strncmp(line, cfgSensorNameTag, cfgSensorNameTagN) == 0) {
				const auto path{ std::format("{}|{}|<voltages>|{}", root, devices[longName], line + cfgSensorNameTagN) };
				// UGLY HACK to deal with SpeedFan [config?] problem
				if (!sensors.contains(path)) {
					values[path] = &sf().volts[volts++], units[path] = Volts;
					sensors.insert(path);
					::OutputDebugString(path.c_str());
				} else
					::OutputDebugString(("Rejected as dupe: " + path).c_str());
				state = WantEnd;
			}
			break;

		case WantEnd:
			if (strncmp(line, cfgEndTag, cfgEndTagN) == 0)
				state = WantTag;
			break;
		}
	}

	trace("SFMonitor found ", sensors.size(), " sensors");
	return sensors.size();
}

/*
	getExecutableDir(string exeToFind) - perform a case-INSENSITIVE search
	for the supplied exe in the running processes list, and return the full
	path of its FOLDER (including the trailing slash), or an empty string
*/
string SFMonitor::getExecutableDir(string_view exeToFind)
{
	// get all processes...
	auto sH = ::CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
	if (sH == INVALID_HANDLE_VALUE)
		return "";	// we're outta here

					// ... and see if SpeedFan is running
	string exePath;
	PROCESSENTRY32 pe { sizeof PROCESSENTRY32 };
	for (auto status = ::Process32First(sH, &pe); status; status = ::Process32Next(sH, &pe)) {
		string exe{ pe.szExeFile };
		std::transform(cbegin(exe), cend(exe), begin(exe), ::tolower);
		if (exe.find(exeToFind) != string::npos) {
			if (auto pH = ::OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pe.th32ProcessID); pH != nullptr) {
				char fullExe[MAX_PATH];
				DWORD nchars = MAX_PATH;
				if (::QueryFullProcessImageName(pH, 0, fullExe, &nchars))
					exePath = string(fullExe, nchars - exeToFind.size());
				::CloseHandle(pH);
			}
			break;
		}
	}
	::CloseHandle(sH);

	return exePath;
}

/*
	type for mapping between IMonitor implementations and "root" names
*/
using string_monitor_map_t = map<string, std::unique_ptr<IMonitor>>;
}

using namespace rxm;

// define our docklet instance data

/*
	Generic caching implementation for GDI+ objects like Pens, Brushes, etc.

	Use by creating a cache mapping a Gdiplus::ARGB -> some GDI+ object PTR

	std::map<Gdiplus::ARGB, std::unique_ptr<Gdiplus::SolidBrush>> brush;

	... and then instantiate the template by supplying the cache and a Color

	return gdip_caching_impl(brush, c); // returns a Gdiplus::SolidBrush*

	NOW using hip map_type concept to constrain the presumed GDI+ container,
	as well as requires clause to guarantee the key_type on said container.
*/

template<typename T>
concept map_type =
std::same_as<T, std::map<typename T::key_type, typename T::mapped_type, typename T::key_compare, typename T::allocator_type>> ||
std::same_as<T, std::unordered_map<typename T::key_type, typename T::mapped_type, typename T::hasher, typename T::key_equal, typename T::allocator_type>>;

template<map_type T>
requires std::same_as<typename T::key_type, ARGB>
const auto gdip_caching_impl(T& gdip_container, Color c)
{
	const auto argb = c.GetValue();
	const auto i = gdip_container.find(argb);
	return i != cend(gdip_container) ?
		i->second.get() :
		gdip_container.emplace(
			argb, make_unique<T::mapped_type::element_type>(c)).first->second.get();
}

static inline auto COLORREF2Color(COLORREF cr)
{
	return Color(GetRValue(cr), GetGValue(cr), GetBValue(cr));
}

/*
	An RXM object represents the linkage between the dock and the docklet, as
	well as being the "root" of the docklet's runtime state information.  In
	our case, it contains everything needed to support both the primary docklet
	function of displaying amalgamated, selected data elements from the current
	running monitor apps, as well as the "Congigure" dialog which allows the
	user to select *which* of these data elements are shown, and how they look.

	The data making this possible is contained in the "monitor" map, linking the
	human-readable names of monitoring apps with the "IMonitor" interfaces that
	abstract their capabilities that RXMDocklet uses, and the array of "Layout"
	objects that control the display of individual values - arranged in 4 pages
	of 8 values, with 2 columns of 4 on each page.
*/
struct RXM {
	using enum LayoutConf;

	const auto CachedBrush(Color c) { return gdip_caching_impl(brush, c); }
	const auto CachedBrush(COLORREF rgb) { return CachedBrush(COLORREF2Color(rgb)); }
	const auto CachedPen(Color c) { return gdip_caching_impl(pen, c); }
	constexpr auto Focus() const { return focusedSensor; }
	constexpr auto DecayFocus() { return --focusedTicksRemaining == 0; }
	constexpr auto Focused() const { return focusedTicksRemaining > 0; }
	constexpr auto StartFocus(int sensor, int n) { focusedSensor = sensor, focusedTicksRemaining = n; }
	const auto FromPath(string path) const { return monitor.find(head(path))->second.get(); }

	HWND hwndDocklet{};					// THIS docklet's HWND
	HINSTANCE hInstance{};				// docklet's DLL HINSTANCE
	int page{};							// current layout page
	int timer{};						// Windows timer id
	int image{};						// background selection
	int fahrenheit{};					// 1=do Fahrenheit conversion
	int focusedSensor{};				// alt-click focused sensor #
	int focusedTicksRemaining{};		// track alt-click focus mode
	string_monitor_map_t monitor;		// RXM Monitor-specific impls
										// Gdiplus cached render env
	unique_ptr<Bitmap> bg_bm, pg_bm[as_int(Pages)];
	unique_ptr<Graphics> bg_g, pg_g[as_int(Pages)];
	StringFormat sf_near{ StringFormatFlagsNoWrap | StringFormatFlagsNoClip, LANG_NEUTRAL };
	StringFormat sf_center{ StringFormatFlagsNoWrap | StringFormatFlagsNoClip, LANG_NEUTRAL };
	StringFormat sf_far{ StringFormatFlagsNoWrap | StringFormatFlagsNoClip, LANG_NEUTRAL };
	Gdiplus::Font f_small{ L"Arial", 15e0F };
	Gdiplus::Font f_large{ L"Arial", 30e0F };
	map<ARGB, unique_ptr<SolidBrush>> brush;
	map<ARGB, unique_ptr<Pen>> pen;
	struct Layout {
		string path;					// our sensor
		COLORREF rgb{ 0 };				// this color
		float last{ -1 };				// last value

		constexpr auto Active() const { return !path.empty(); }
		void Assign(COLORREF c) { rgb = c; }
		void Assign(string_view p, COLORREF c) { path = p, rgb = c; }
		void Clear() { path.clear(), rgb = 0, last = -1; }
		auto Live(RXM* rxm) const {
			const auto&& mi = rxm->monitor.find(head(path));
			return mi != cend(rxm->monitor) ? mi->second.get()->Sensors().contains(path) : false;
		}
		void Render(RXM* rxm, Graphics& g, const Gdiplus::Font& f, const RectF& r, const StringFormat& sf, bool unitString = false) {
			// display individual sensor with supplied GdiPlus formatting & attributes AS REQUIRED
			if (Active()) {
				wstring t{ L'-' };
				if (Live(rxm)) {
					auto m = rxm->FromPath(path);
					if (m->RefreshNeeded())
						m->Refresh();
					last = m->SensorValue(path);
					t = utf16StringFromUTF8((unitString ?
						m->SensorUnitString(path,  rxm->fahrenheit == 1):
						m->SensorValueString(path, rxm->fahrenheit == 1)).c_str());
				} else
					trace("Sensor ", path, " is UNDEAD!");
				g.DrawString(t.c_str(), -1, &f, r, &sf, rxm->CachedBrush(rgb));
			}
		}
		auto UpdateRequired(RXM* rxm) const { return Active() && Live(rxm) && rxm->FromPath(path)->SensorValue(path) != last; }
	} layout[as_int(Pages)][as_int(LayoutsPerPage)]; // sensor layouts (all pages)
};

// RXMConfigure dialog interface

class RXMConfigure : public CDialogEx
{
	using enum LayoutConf;

	//DECLARE_DYNAMIC(RXMConfigure)

	RXM* rxm;							// OUR docklet instance data

	map<HTREEITEM, sensor_t> pathFromTree;
	map<sensor_t, HTREEITEM> treeFromPath;

	static constexpr int colorControlID[as_int(LayoutsPerPage)] {
		IDC_COLOR1, IDC_COLOR2, IDC_COLOR3, IDC_COLOR4,
		IDC_COLOR5, IDC_COLOR6, IDC_COLOR7, IDC_COLOR8
	};
	static constexpr int editControlID[as_int(LayoutsPerPage)] {
		IDC_SENSOR1, IDC_SENSOR2, IDC_SENSOR3, IDC_SENSOR4,
		IDC_SENSOR5, IDC_SENSOR6, IDC_SENSOR7, IDC_SENSOR8
	};

	void assignSensor(int sensor);
	void assignColor(int sensor);
	void initializeBackgroundList();
	void initializeSensors();
	void initializeSensorTree();
	void locateSensor(int sensor);
	void unassignSensor(int sensor);

	constexpr auto isFahrenheit() const { return rxm->fahrenheit == 1; }

public:
	RXMConfigure() = delete;
	RXMConfigure(RXM* rxm, CWnd* pParent = nullptr)
		: CDialogEx(RXMConfigure::IDD, pParent)
		, rxm(rxm)
		, celsiusOrFahrenheit(0) {}
	virtual ~RXMConfigure() = default;

// Dialog Data
	enum { IDD = IDD_RXM_CONFIGURE };

protected:
	int celsiusOrFahrenheit;

	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

public:
	CTabCtrl SensorTab;
	CTreeCtrl SensorTree;
	CComboBox Background;
	CMFCColorButton setColor;
	CMFCButton assign1, assign2, assign3, assign4, assign5, assign6, assign7, assign8;
	CMFCButton locate1, locate2, locate3, locate4, locate5, locate6, locate7, locate8;

public:
	virtual BOOL OnInitDialog();
	afx_msg void OnBnClickedAssign1() { assignSensor(0); }
	afx_msg void OnBnClickedAssign2() { assignSensor(1); }
	afx_msg void OnBnClickedAssign3() { assignSensor(2); }
	afx_msg void OnBnClickedAssign4() { assignSensor(3); }
	afx_msg void OnBnClickedAssign5() { assignSensor(4); }
	afx_msg void OnBnClickedAssign6() { assignSensor(5); }
	afx_msg void OnBnClickedAssign7() { assignSensor(6); }
	afx_msg void OnBnClickedAssign8() { assignSensor(7); }
	afx_msg void OnBnClickedColor1() { assignColor(0); }
	afx_msg void OnBnClickedColor2() { assignColor(1); }
	afx_msg void OnBnClickedColor3() { assignColor(2); }
	afx_msg void OnBnClickedColor4() { assignColor(3); }
	afx_msg void OnBnClickedColor5() { assignColor(4); }
	afx_msg void OnBnClickedColor6() { assignColor(5); }
	afx_msg void OnBnClickedColor7() { assignColor(6); }
	afx_msg void OnBnClickedColor8() { assignColor(7); }
	afx_msg void OnBnClickedLocate1() { locateSensor(0); }
	afx_msg void OnBnClickedLocate2() { locateSensor(1); }
	afx_msg void OnBnClickedLocate3() { locateSensor(2); }
	afx_msg void OnBnClickedLocate4() { locateSensor(3); }
	afx_msg void OnBnClickedLocate5() { locateSensor(4); }
	afx_msg void OnBnClickedLocate6() { locateSensor(5); }
	afx_msg void OnBnClickedLocate7() { locateSensor(6); }
	afx_msg void OnBnClickedLocate8() { locateSensor(7); }
	afx_msg void OnEnChangeSensor1() { unassignSensor(0); }
	afx_msg void OnEnChangeSensor2() { unassignSensor(1); }
	afx_msg void OnEnChangeSensor3() { unassignSensor(2); }
	afx_msg void OnEnChangeSensor4() { unassignSensor(3); }
	afx_msg void OnEnChangeSensor5() { unassignSensor(4); }
	afx_msg void OnEnChangeSensor6() { unassignSensor(5); }
	afx_msg void OnEnChangeSensor7() { unassignSensor(6); }
	afx_msg void OnEnChangeSensor8() { unassignSensor(7); }
	afx_msg void OnCbnSelchangeBackground();
	afx_msg void OnTcnSelchangeSensorTab(NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void OnBnClickedCelsius();
	afx_msg void OnBnClickedFahrenheit();
	afx_msg void OnBnClickedSavelocal();
	afx_msg void OnTvnGetInfoTipSensorTree(NMHDR *pNMHDR, LRESULT *pResult);
};

// RXMConfigure dialog support functions

constexpr void initializeRXM(RXM* rxm, HWND hwndDocklet, HINSTANCE hInstance)
{
	rxm->hwndDocklet = hwndDocklet, rxm->hInstance = hInstance;
}

static void initializeGdipRenderCache(RXM* rxm)
{
	rxm->bg_bm = make_unique<Bitmap>(128, 128, PixelFormat32bppARGB);
	rxm->bg_g = make_unique<Graphics>(rxm->bg_bm.get());
	for (auto& bm : rxm->pg_bm) {
		bm = make_unique<Bitmap>(128, 128, PixelFormat32bppARGB);
		rxm->pg_g[std::distance(rxm->pg_bm, &bm)] = make_unique<Graphics>(bm.get());
	}
	rxm->sf_near.SetTrimming(StringTrimmingNone);
	rxm->sf_near.SetLineAlignment(StringAlignmentCenter);
	rxm->sf_near.SetAlignment(StringAlignmentNear);
	rxm->sf_center.SetTrimming(StringTrimmingNone);
	rxm->sf_center.SetLineAlignment(StringAlignmentCenter);
	rxm->sf_center.SetAlignment(StringAlignmentCenter);
	rxm->sf_far.SetTrimming(StringTrimmingNone);
	rxm->sf_far.SetLineAlignment(StringAlignmentCenter);
	rxm->sf_far.SetAlignment(StringAlignmentFar);
}

static void loadProfile(RXM* rxm, const char* ini, const char* iniGroup)
{
	// slurp in sensor, color, and temperature settings
	for (auto p = 0; p < as_int(LayoutConf::Pages); ++p)
		for (auto s = 0; s < as_int(LayoutConf::LayoutsPerPage); ++s) {
			string k1{ std::format("Sensor{}-{}", p + 1, s + 1) };
			if (char b[MAX_PATH]; ::GetPrivateProfileString(iniGroup, k1.c_str(), "", b, MAX_PATH, ini)) {
				string k2{ std::format("Color{}-{}", p + 1, s + 1) };
				rxm->layout[p][s].Assign(b, ::GetPrivateProfileInt(iniGroup, k2.c_str(), 0, ini));
			}
		}

	const auto j = ::GetPrivateProfileInt(iniGroup, "Fahrenheit", 0, ini);
	rxm->fahrenheit = j == 0 ? 0 : 1;
	const auto k = ::GetPrivateProfileInt(iniGroup, "Background", 0, ini);
	rxm->image = __min(__max((int)k, 0), as_int(LayoutConf::BackgroundImages) - 1);
}

constexpr auto pageIsActive(RXM* rxm)
{
	return std::any_of(cbegin(rxm->layout[rxm->page]), cend(rxm->layout[rxm->page]), [](auto l) {
		return l.Active();
	});
}

static void renderBackground(RXM* rxm)
{
	auto& g{ *rxm->bg_g };
	auto drawGrid = [&](auto& argb) {
		auto p = rxm->CachedPen(argb);
		for (auto x = 16; x < 128; x += 16)
			g.DrawLine(p, x - 0.5f, -0.5f, x - 0.5f, 127.5f);
		for (auto y = 16; y < 128; y += 16)
			g.DrawLine(p, -0.5f, y - 0.5f, 127.5f, y - 0.5f);
	};
	auto drawRect = [&](auto& argb) {
		g.DrawRectangle(rxm->CachedPen(argb), 0, 0, 127, 127);
	};
	auto fillRect = [&](auto& argb) {
		g.Clear(argb);
	};
	static const Color Black{ 255, 0, 0, 0 };
	static const Color Clear{ 0, 0, 0, 0 };
	static const Color SoftBlue{ 128, 0, 128, 255 };
	static const Color SoftGray{ 128, 0, 0, 0 };
	static const Color White{ 255, 255, 255, 255 };
	static const Color Yellow{ 255, 250, 242, 173 };

	// [re-]display current background
	switch (rxm->image) {
	// Black
	case 0: fillRect(Black); break;
	// Clear
	case 1: fillRect(Clear); break;
	// Clear with Border
	case 2: fillRect(Clear); drawRect(Black); break;
	// Clear with Grid
	case 3: fillRect(Clear); drawRect(Black); drawGrid(SoftGray); break;
	// White
	case 4: fillRect(White); break;
	// White with Border
	case 5: fillRect(White); drawRect(SoftBlue); break;
	// White with Grid
	case 6: fillRect(White); drawRect(SoftBlue); drawGrid(SoftBlue); break;
	// Yellow
	case 7: fillRect(Yellow); break;
	// Yellow with Border
	case 8: fillRect(Yellow); drawRect(SoftBlue); break;
	// Yellow with Grid
	case 9: fillRect(Yellow); drawRect(SoftBlue); drawGrid(SoftBlue); break;
	}

	DockletSetImage(rxm->hwndDocklet, rxm->bg_bm.get(), FALSE);
}

static void renderPage(RXM* rxm, RenderType render = RenderType::Normal, POINT* pt = nullptr, SIZE* sz = nullptr)
{
	// display all sensors on current page AS REQUIRED
	if (render != RenderType::StartFocus &&
		render != RenderType::EndFocus &&
		render != RenderType::Forced &&
		!rxm->Focused() &&
		std::none_of(cbegin(rxm->layout[rxm->page]), cend(rxm->layout[rxm->page]), [rxm](auto& l) {
			return l.UpdateRequired(rxm); }))
		return; // (nothing to do)

	// check for (and handle) render state machine state changes (I)
	if (render == RenderType::EndFocus || render == RenderType::Forced)
		rxm->StartFocus(0, 0);

	// yup, some kind of update *is* required, build rendering environment, part I...
	static const RectF zones[]{
		{ 0, 0, 128, 32 }, { 0, 32, 128, 32 }, { 0, 64, 128, 32 }, { 0, 96, 128, 32 },
		{ 0, 0, 128, 64 }, { 0, 64, 128, 64 }
	};
	auto& g{ *rxm->pg_g[rxm->page] };
	static const Color zeroed{ 0, 0, 0, 0 };
	g.Clear(zeroed);
	g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

	// build rendering environment, part II...
	auto& layouts = rxm->layout[rxm->page];
	const auto n = std::count_if(cbegin(layouts), cend(layouts), [](auto& l) { return l.Active(); });
	const auto n_effective = render == RenderType::StartFocus || rxm->Focused() ? 1 : n;
	// (select appropriate Font for below "dynamic" layout)
	auto& f{ n_effective > 2 ? rxm->f_small : rxm->f_large };
	auto rendered = 0;

	// workhorse lambda for matching click to sensor
	auto matchRectF = [&](const auto& pt, const auto& sz) {
		const auto cx = (REAL)sz.cx / 128;
		const auto cy = (REAL)sz.cy / 128;
		std::remove_cvref_t<decltype(zones)> z;
		std::copy(begin(zones), end(zones), begin(z));
		for (auto& r : z)
			r.Width *= cx, r.Height *= cy;
		if (const auto x = REAL(pt.x), y = REAL(pt.y); n > 2) {
			for (auto i = 0; i < 4; ++i)
				if (z[i].Contains(x, y))
					return pt.x < z[i].Width / 2 ? i : i + 4;
		} else
			for (auto i = 4; i < 6; ++i)
				if (z[i].Contains(x, y))
					return i == 4 ? 0 : 1;
		return -1; // (indicate NO match found)
	};

	// check for (and handle) render state machine state changes (II)
	if (render == RenderType::StartFocus) {
		const auto i = matchRectF(*pt, *sz);
		if (i < 0 || !(layouts[i].Active() && layouts[i].Live(rxm)))
			return; // (treat failed and/or useless match as a no-op)
		rxm->StartFocus(i, 5);
	}

	// workhorse lambda for "dynamic" layout based on # of active Layouts in the page...
	auto doSingleLayout = [&](auto& l, auto n) {
		const auto i = std::distance(layouts, &l);
		StringFormat& sf{
			n <= 2 ? rxm->sf_center :
			i < as_int(LayoutConf::LayoutsPerPage) / 2 ? rxm->sf_near :
			rxm->sf_far };
		switch (n) {
		case 1:
			// "zoomed": 1 sensor, value on top, unit on bottom
			l.Render(rxm, g, f, zones[4], sf), ++rendered;
			l.Render(rxm, g, f, zones[5], sf, true);
			break;
		case 2:
			// "large": 2 sensors, 1 on top, 1 on bottom
			l.Render(rxm, g, f, zones[rendered ? 5 : 4], sf), ++rendered;
			break;
		default:
			// "2-column": up to 4 sensors on left, up to 4 sensors on right
			l.Render(rxm, g, f, zones[i & 3], sf), ++rendered;
			break;
		}
	};

	if (rxm->Focused()) {
		// for single "focused" layout
		if (auto& l = layouts[rxm->Focus()]; l.Active())
			doSingleLayout(l, 1);
	} else
		// FOREACH [in-use] Layout on current page...
		for (auto& l : layouts)
			if (l.Active())
				doSingleLayout(l, n);

	DockletSetImageOverlay(rxm->hwndDocklet, rxm->pg_bm[rxm->page].get(), FALSE);
}

static void saveProfile(RXM* rxm, const char* ini, const char* iniGroup, bool asDefault = false)
{
	// handle "save local defaults" AS REQUIRED
	if (asDefault)
		WritePrivateProfileInt(iniGroup, "ForceDockletDefaults", 1, ini);
	// stash sensor, color, and temperature settings
	for (auto p = 0; p < as_int(LayoutConf::Pages); ++p)
		for (auto s = 0; s < as_int(LayoutConf::LayoutsPerPage); ++s) {
			const auto& l = rxm->layout[p][s];
			string
				k1{ std::format("Sensor{}-{}", p + 1, s + 1) },
				k2{ std::format("Color{}-{}", p + 1, s + 1) };
			if (l.Active()) {
				::WritePrivateProfileString(iniGroup, k1.c_str(), l.path.c_str(), ini);
				WritePrivateProfileInt(iniGroup, k2.c_str(), l.rgb, ini);
			} else {
				::WritePrivateProfileString(iniGroup, k1.c_str(), nullptr, ini);
				::WritePrivateProfileString(iniGroup, k2.c_str(), nullptr, ini);
			}
		}
	WritePrivateProfileInt(iniGroup, "Fahrenheit", rxm->fahrenheit, ini);
	WritePrivateProfileInt(iniGroup, "Background", rxm->image, ini);
}

// ObjectDock SDK 1.0 callbacks

RXM* CALLBACK OnCreateRXM(HWND hwndDocklet, HINSTANCE hInstance, char *szIni, char *szIniGroup)
{
	::OutputDebugString("OnCreateRXM...");
	auto rxm = make_unique<RXM>();
	initializeRXM(rxm.get(), hwndDocklet, hInstance);

	// create the specialized Monitors...
	rxm->monitor.emplace("ABM", make_unique<ABMonitor>("ABM", "AfterBurner"));
	rxm->monitor.emplace("CT", make_unique<CTMonitor>("CT", "Core Temp"));
	rxm->monitor.emplace("GPUZ", make_unique<GPUZMonitor>("GPUZ", "GPU-Z"));
	rxm->monitor.emplace("HWIM", make_unique<HWiMonitor>("HWIM", "HWiNFO"));
	rxm->monitor.emplace("HWM", make_unique<HWMonitor>("HWM", "HWMonitor"));
	rxm->monitor.emplace("SF", make_unique<SFMonitor>("SF", "SpeedFan"));
	// ... and initialize the ones that are [initially] present
	for (auto& [_, m] : rxm->monitor)
		m->Refresh();

	if (rxm->monitor.empty())
		DockletSetLabel(hwndDocklet, const_cast<char*>("Start a Monitor application!"));

	// load profile (if there is one)...
	string_view ini(szIni ? szIni : ""), iniGroup(szIniGroup ? szIniGroup : "");
	if (!ini.empty() && !iniGroup.empty())
		loadProfile(rxm.get(), ini.data(), iniGroup.data()); // zero-terminated!
	else if (!rxm->monitor.empty())
		DockletSetLabel(hwndDocklet, const_cast <char*>("Configure Docklet!"));
	// ... initialize [GdiPlus] cached rendering env...
	initializeGdipRenderCache(rxm.get());
	// ... and set background
	renderBackground(rxm.get());

	// display everything ONCE...
	renderPage(rxm.get());

	// ... and start timer so it happens periodically
	rxm->timer = ::SetTimer(hwndDocklet, 43, 1*1000, nullptr);
	return rxm.release();
}

void CALLBACK OnDestroyRXM(RXM* rxm, HWND hwndDocklet)
{
	::OutputDebugString("OnDestroyRXM...");
	// whack timer AS REQUIRED
	if (rxm->timer > 0)
		::KillTimer(rxm->hwndDocklet, rxm->timer);
	delete rxm;	// free docklet instance data
}

void CALLBACK OnGetInformation(char *szName, char *szAuthor, int *iVersion, char *szNotes)
{
	// init info values
	strcpy(szName, "RXM Docklet");
	strcpy(szAuthor, "Robert Roessler");
	*iVersion = 200;
	strcpy(szNotes,
		"ObjectDock display docklet\r\n"
		"for GPU-Z (www.techpowerup.com/gpuz/),\r\n"
		"HWiNFO (www.hwinfo.com),\r\n"
		"MSI Afterburner (gaming.msi.com/features/afterburner),\r\n"
		"Core Temp (www.alcpu.com/CoreTemp/),\r\n"
		"SpeedFan (www.almico.com/speedfan.php),\r\n"
		"and CPUID HWMonitor (www.cpuid.com).\r\n"
		"\r\n"
		"Copyright (C) 2009-2021 Robert Roessler\r\n"
		"www.rftp.com");
}

BOOL CALLBACK OnLeftButtonClick(RXM* rxm, POINT *pt, SIZE *sz)
{
	// check status of ALT (aka MENU) modifier key...
	if (!rxm->Focused() && ::GetAsyncKeyState(VK_MENU) < 0)
		// switch into "zoomed"/"focused" mode for a few seconds
		renderPage(rxm, RenderType::StartFocus, pt, sz);
	else {
		// show "next" page of sensors (N.B. - "Pages" must be a power of 2)
		// (click moves "forward" in set of pages, shift-click moves "back")
		const auto delta = ::GetAsyncKeyState(VK_SHIFT) < 0 ? -1 : 1;
		const auto i = rxm->page;
		do
			rxm->page = (rxm->page + delta) & (as_int(LayoutConf::Pages) - 1);
		while (!pageIsActive(rxm) && rxm->page != i);
		if (rxm->page != i)
			renderPage(rxm, RenderType::Forced);
	}
	return TRUE;
}

void CALLBACK OnProcessMessage(RXM* rxm, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	// process message... like, say, WM_TIMER
	switch (uMsg) {
	// WM_TIMER: update sensor display(s) AS REQUIRED
	case WM_TIMER:
		if (wParam == 43)
			renderPage(rxm, rxm->Focused() && rxm->DecayFocus() ? RenderType::EndFocus : RenderType::Normal);
		break;

	// WM_POWERBROADCAST: on resume, re-enumerate sensors AS REQUIRED
	case WM_POWERBROADCAST:
		if (wParam == PBT_APMRESUMEAUTOMATIC)
			for (auto& [_, m] : rxm->monitor)
				m->Refresh();
		break;
	}
}

BOOL CALLBACK OnRightButtonClick(RXM* rxm, POINT *ptCursor, SIZE *sizeDocklet)
{
	// build and display "RXM" menu
	CPoint p;
	::GetCursorPos(&p);
	CMenu m;
	m.CreatePopupMenu();
	m.AppendMenu(MF_STRING|MF_ENABLED, 1, "Configure...");
	m.AppendMenu(MF_STRING|MF_ENABLED, 2, "Refresh");
	m.AppendMenu(MF_STRING|MF_DISABLED, 3, "About RXM Docklet");
	m.AppendMenu(MF_SEPARATOR);
	m.AppendMenu(MF_STRING|MF_ENABLED, 4, "Dock Menu");
	BOOL rv = FALSE;

	DockletLockMouseEffect(rxm->hwndDocklet, TRUE);
	const int i = ::TrackPopupMenu(m, TPM_RETURNCMD|TPM_NONOTIFY|TPM_RIGHTBUTTON|TPM_LEFTALIGN|TPM_TOPALIGN,
		p.x, p.y, 0, rxm->hwndDocklet, nullptr);
	// so, what to do...
	switch (i) {
	case 1: {
		// configure docklet
		RXMConfigure cfg(rxm);
		const auto oldPage = rxm->page, oldFahrenheit = rxm->fahrenheit, oldImage = rxm->image;
		decltype(rxm->layout) oldLayout;
		std::copy(decayed_begin(rxm->layout), decayed_end(rxm->layout), decayed_begin(oldLayout));
		if (cfg.DoModal() != IDOK) {
			// user changed their mind, put it all back... AS REQUIRED
			std::copy(decayed_begin(oldLayout), decayed_end(oldLayout), decayed_begin(rxm->layout));
			// ... and try to save a little [resource allocation] work
			if (rxm->image != oldImage) {
				rxm->image = oldImage;
				renderBackground(rxm);
			}
			rxm->page = oldPage, rxm->fahrenheit = oldFahrenheit;
			renderPage(rxm, RenderType::Forced);
		}
		rv = TRUE;
		break;
	}
	case 2:
		// "refresh" (actually, re-enumerate) Monitors
		for (auto& [_, m] : rxm->monitor)
			m->Refresh();
		rv = TRUE;
		break;
	case 3:
		// display About dialog
		rv = TRUE;
		break;
	case 4:
		// [just] quit and let [Rocket] Dock display its menu
		break;
	default:
		// quit, but NO RocketDock menu
		rv = TRUE;
		break;
	}
	DockletLockMouseEffect(rxm->hwndDocklet, FALSE);

	return rv;
}

void CALLBACK OnSaveRXM(RXM* rxm, char *szIni, char *szIniGroup, BOOL bIsForExport)
{
	saveProfile(rxm, szIni, szIniGroup);
}

// RXMConfigure dialog implementation

void RXMConfigure::DoDataExchange(CDataExchange* pDX)
{
	CDialogEx::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_SENSOR_TAB, SensorTab);
	DDX_Control(pDX, IDC_SENSOR_TREE, SensorTree);
	DDX_Control(pDX, IDC_BACKGROUND, Background);
	DDX_Radio(pDX, IDC_CELSIUS, celsiusOrFahrenheit);
	DDX_Control(pDX, IDC_COLOR1, setColor);
	DDX_Control(pDX, IDC_ASSIGN1, assign1);
	DDX_Control(pDX, IDC_ASSIGN2, assign2);
	DDX_Control(pDX, IDC_ASSIGN3, assign3);
	DDX_Control(pDX, IDC_ASSIGN4, assign4);
	DDX_Control(pDX, IDC_ASSIGN5, assign5);
	DDX_Control(pDX, IDC_ASSIGN6, assign6);
	DDX_Control(pDX, IDC_ASSIGN7, assign7);
	DDX_Control(pDX, IDC_ASSIGN8, assign8);
	DDX_Control(pDX, IDC_LOCATE1, locate1);
	DDX_Control(pDX, IDC_LOCATE2, locate2);
	DDX_Control(pDX, IDC_LOCATE3, locate3);
	DDX_Control(pDX, IDC_LOCATE4, locate4);
	DDX_Control(pDX, IDC_LOCATE5, locate5);
	DDX_Control(pDX, IDC_LOCATE6, locate6);
	DDX_Control(pDX, IDC_LOCATE7, locate7);
	DDX_Control(pDX, IDC_LOCATE8, locate8);
}

void RXMConfigure::assignColor(int sensor)
{
	auto& l = rxm->layout[rxm->page][sensor];
	if (!l.Active())
		return;	// nothing to do

	l.Assign(((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->GetColor());
	renderPage(rxm, RenderType::Forced);
}

void RXMConfigure::assignSensor(int sensor)
{
	// set this sensor's corresponding hardware sensor...
	auto h = SensorTree.GetSelectedItem();
	if (h == nullptr || SensorTree.ItemHasChildren(h))
		return;	// we're outta here

	// ... from the displayed sensor tree
	const auto& path = pathFromTree[h];
	const auto elidedPath = string(head(path)) + (const char*)u8"…" + string(tail(path));
	((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText(elidedPath.c_str());
	rxm->layout[rxm->page][sensor].Assign(path, ((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->GetColor());
	renderPage(rxm);
}

void RXMConfigure::initializeBackgroundList()
{
	static const constinit char* b[as_int(LayoutConf::BackgroundImages)]{
		"Black",
		"Clear", "Clear with Border", "Clear with Grid",
		"White", "White with Border", "White with Grid",
		"Yellow", "Yellow with Border", "Yellow with Grid"
	};
	auto* cb = (CComboBox*)GetDlgItem(IDC_BACKGROUND);
	cb->SetRedraw(FALSE);
	for (const auto& t : b)
		cb->AddString(t);
	cb->SetRedraw(TRUE);
}

void RXMConfigure::initializeSensors()
{
	for (auto& l : rxm->layout[rxm->page])
		if (const auto sensor = std::distance(rxm->layout[rxm->page], &l); l.Active()) {
			// display this sensor's color...
			((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->SetColor(l.rgb);
			// ... and [somewhat descriptive] name
			const auto elidedPath = string(head(l.path)) + (const char*)u8"…" + string(tail(l.path));
			((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText(elidedPath.c_str());
		} else {
			// (nothing [much] to do)
			((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->SetColor(RGB(192, 192, 192));
			((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText("");
		}
}

void RXMConfigure::initializeSensorTree()
{
	static const std::regex pathSeparator("\\|");

	// FOREACH specialized Monitor...
	for (const auto& [_, m] : rxm->monitor) {
		const auto& sensors = m->Sensors();
		if (sensors.empty())
			continue;
		const int n = split(*cbegin(sensors), pathSeparator).size();
		vector<string> treeName(n, "");
		vector<HTREEITEM> treeHandle(n, nullptr);
		// ... build the sensor tree from the [ordered] set of paths
		for (const auto& s : sensors) {
			const auto path = split(s, pathSeparator);
			for (auto i = 0; i < n; ++i)
				if (path[i] != treeName[i])
					if (i == 0)
						treeHandle[0] = SensorTree.InsertItem(m->DisplayName().c_str(), TVI_ROOT),
						treeName[0] = path[0];
					else
						treeHandle[i] = SensorTree.InsertItem(path[i].c_str(), treeHandle[i - 1]),
						treeName[i] = path[i], fill(begin(treeName) + i + 1, end(treeName), "");

			// set up path <-> tree LEAF mappings
			const auto h = treeHandle[n - 1];
			pathFromTree[h] = s;
			treeFromPath[s] = h;
		}

		SensorTree.Expand(treeHandle[0], TVE_EXPAND);
	}
}

void RXMConfigure::locateSensor(int sensor)
{
	// set this sensor's corresponding hardware tree location...
	const auto& l = rxm->layout[rxm->page][sensor];
	if (!l.Active() || !l.Live(rxm))
		return;	// we're outta here

	// ... from the assigned sensor path (also set focus)
	const auto h = treeFromPath[l.path];
	SensorTree.SetFocus();
	SensorTree.EnsureVisible(h);
	SensorTree.SelectItem(h);
}

void RXMConfigure::unassignSensor(int sensor)
{
	// "free" (unassign) this sensor
	((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->SetColor(RGB(192, 192, 192));
	((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText("");
	rxm->layout[rxm->page][sensor].Clear();
	renderPage(rxm, RenderType::Forced);
}

BEGIN_MESSAGE_MAP(RXMConfigure, CDialogEx)
	ON_BN_CLICKED(IDC_ASSIGN1, RXMConfigure::OnBnClickedAssign1)
	ON_BN_CLICKED(IDC_ASSIGN2, RXMConfigure::OnBnClickedAssign2)
	ON_BN_CLICKED(IDC_ASSIGN3, RXMConfigure::OnBnClickedAssign3)
	ON_BN_CLICKED(IDC_ASSIGN4, RXMConfigure::OnBnClickedAssign4)
	ON_BN_CLICKED(IDC_ASSIGN5, RXMConfigure::OnBnClickedAssign5)
	ON_BN_CLICKED(IDC_ASSIGN6, RXMConfigure::OnBnClickedAssign6)
	ON_BN_CLICKED(IDC_ASSIGN7, RXMConfigure::OnBnClickedAssign7)
	ON_BN_CLICKED(IDC_ASSIGN8, RXMConfigure::OnBnClickedAssign8)
	ON_BN_CLICKED(IDC_COLOR1, RXMConfigure::OnBnClickedColor1)
	ON_BN_CLICKED(IDC_COLOR2, RXMConfigure::OnBnClickedColor2)
	ON_BN_CLICKED(IDC_COLOR3, RXMConfigure::OnBnClickedColor3)
	ON_BN_CLICKED(IDC_COLOR4, RXMConfigure::OnBnClickedColor4)
	ON_BN_CLICKED(IDC_COLOR5, RXMConfigure::OnBnClickedColor5)
	ON_BN_CLICKED(IDC_COLOR6, RXMConfigure::OnBnClickedColor6)
	ON_BN_CLICKED(IDC_COLOR7, RXMConfigure::OnBnClickedColor7)
	ON_BN_CLICKED(IDC_COLOR8, RXMConfigure::OnBnClickedColor8)
	ON_BN_CLICKED(IDC_LOCATE1, RXMConfigure::OnBnClickedLocate1)
	ON_BN_CLICKED(IDC_LOCATE2, RXMConfigure::OnBnClickedLocate2)
	ON_BN_CLICKED(IDC_LOCATE3, RXMConfigure::OnBnClickedLocate3)
	ON_BN_CLICKED(IDC_LOCATE4, RXMConfigure::OnBnClickedLocate4)
	ON_BN_CLICKED(IDC_LOCATE5, RXMConfigure::OnBnClickedLocate5)
	ON_BN_CLICKED(IDC_LOCATE6, RXMConfigure::OnBnClickedLocate6)
	ON_BN_CLICKED(IDC_LOCATE7, RXMConfigure::OnBnClickedLocate7)
	ON_BN_CLICKED(IDC_LOCATE8, RXMConfigure::OnBnClickedLocate8)
	ON_EN_CHANGE(IDC_SENSOR1, RXMConfigure::OnEnChangeSensor1)
	ON_EN_CHANGE(IDC_SENSOR2, RXMConfigure::OnEnChangeSensor2)
	ON_EN_CHANGE(IDC_SENSOR3, RXMConfigure::OnEnChangeSensor3)
	ON_EN_CHANGE(IDC_SENSOR4, RXMConfigure::OnEnChangeSensor4)
	ON_EN_CHANGE(IDC_SENSOR5, RXMConfigure::OnEnChangeSensor5)
	ON_EN_CHANGE(IDC_SENSOR6, RXMConfigure::OnEnChangeSensor6)
	ON_EN_CHANGE(IDC_SENSOR7, RXMConfigure::OnEnChangeSensor7)
	ON_EN_CHANGE(IDC_SENSOR8, RXMConfigure::OnEnChangeSensor8)
	ON_BN_CLICKED(IDC_CELSIUS, &RXMConfigure::OnBnClickedCelsius)
	ON_BN_CLICKED(IDC_FAHRENHEIT, &RXMConfigure::OnBnClickedFahrenheit)
	ON_BN_CLICKED(IDC_SAVELOCAL, RXMConfigure::OnBnClickedSavelocal)
	ON_CBN_SELCHANGE(IDC_BACKGROUND, RXMConfigure::OnCbnSelchangeBackground)
	ON_NOTIFY(TCN_SELCHANGE, IDC_SENSOR_TAB, RXMConfigure::OnTcnSelchangeSensorTab)
	ON_NOTIFY(TVN_GETINFOTIP, IDC_SENSOR_TREE, RXMConfigure::OnTvnGetInfoTipSensorTree)
END_MESSAGE_MAP()

// RXMConfigure message handlers

void RXMConfigure::OnBnClickedCelsius()
{
	rxm->fahrenheit = celsiusOrFahrenheit = 0;
	renderPage(rxm, RenderType::Forced);
}

void RXMConfigure::OnBnClickedFahrenheit()
{
	rxm->fahrenheit = celsiusOrFahrenheit = 1;
	renderPage(rxm, RenderType::Forced);
}

void RXMConfigure::OnBnClickedSavelocal()
{
	char pathN[MAX_PATH];
	DockletGetRelativeFolder(rxm->hwndDocklet, pathN);
	saveProfile(rxm, "Docklets\\Defaults.ini", (pathN + "RXMDocklet.dll"s).c_str(), true);
}

void RXMConfigure::OnCbnSelchangeBackground()
{
	// user has just selected a new background
	rxm->image = Background.GetCurSel();
	renderBackground(rxm);
}

BOOL RXMConfigure::OnInitDialog()
{
	CDialogEx::OnInitDialog();

	initializeSensorTree();

	// init tab control and "style" manager
	CMFCVisualManager::SetDefaultManager(RUNTIME_CLASS(CMFCVisualManagerWindows));
	SensorTab.InsertItem(0, "Page 1");
	SensorTab.InsertItem(1, "Page 2");
	SensorTab.InsertItem(2, "Page 3");
	SensorTab.InsertItem(3, "Page 4");
	SensorTab.SetCurSel(rxm->page);

	// init Sensor, Temperature, and Background controls
	initializeSensors();
	initializeBackgroundList();
	celsiusOrFahrenheit = rxm->fahrenheit;
	Background.SetCurSel(rxm->image);

	UpdateData(FALSE);

	// "locate" 1st in-use sensor in tree
	for (auto& l : rxm->layout[0])
		if (l.Active())
			return locateSensor(std::distance(rxm->layout[0], &l)), FALSE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void RXMConfigure::OnTcnSelchangeSensorTab(NMHDR *pNMHDR, LRESULT *pResult)
{
	rxm->page = SensorTab.GetCurSel();
	initializeSensors();
	renderPage(rxm, RenderType::Forced);
	*pResult = 0;
}

void RXMConfigure::OnTvnGetInfoTipSensorTree(NMHDR *pNMHDR, LRESULT *pResult)
{
	auto pGetInfoTip = reinterpret_cast<LPNMTVGETINFOTIP>(pNMHDR);
	// (make sure we are dealing with a LEAF)
	if (auto h = pGetInfoTip->hItem; !SensorTree.ItemHasChildren(h)) {
		const auto& path = pathFromTree[h];
		const auto m = rxm->FromPath(path);
		const auto isF = isFahrenheit();
		std::format_to_n(pGetInfoTip->pszText, pGetInfoTip->cchTextMax, "{} {}", m->SensorValueString(path, isF), m->SensorUnitString(path, isF));
	}
	*pResult = 0;
}

// RXMDockletApp (the DLL)

class RXMDockletApp : public CWinApp {
public:
	RXMDockletApp() {}

	// Overrides
public:
	virtual BOOL InitInstance() override {
		CWinApp::InitInstance();
		return TRUE;
	}

	DECLARE_MESSAGE_MAP()
};

BEGIN_MESSAGE_MAP(RXMDockletApp, CWinApp)
END_MESSAGE_MAP()

// RXMDockletApp singleton

RXMDockletApp theApp;
