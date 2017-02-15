/*
	RXMDocklet.cpp - "X" Monitor Docklet [DLL] implementation(s)

	Copyright(c) 2009-2017, Robert Roessler
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
#include <sstream>
#include <fstream>
#include <iomanip>
#include <memory>
#include <set>
#include <map>
#include <vector>
#include <regex>
#include "sdk/DockletSDK.h"
#include "TlHelp32.h"
#include "resource.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

using namespace std;

/*
	The rxm namespace contains all primary and supporting logic for
	accessing and displaying the provider-specific shared memory based
	data representations of some common "hardware monitors"... the
	expected and supported client is the RXMDocklet plugin, compatible
	with the ObjectDock Docklet SDK v1.0 interface specification(**).

	Currently supported apps include GPU-Z, HWiNFO(*), CPUID HWMonitor,
	MSI Afterburner, and SpeedFan.

	* - note that HWiNFO support at this time is only available in the
	pre-built binary versions of RXMDocklet.

	** - this has only been tested/used with the [final] 1.3.5 version
	of RocketDock... and even though the RocketDock download page says
	that it is "unsupported" on 64-bit versions of Windows, it works!
*/
namespace rxm {

/*
	Definition and implementation of simple [threading-aware] spinlock
*/
class RSpinLock {
public:
	inline void lock() {
		// try simple lock...
		while (lock_.test_and_set(std::memory_order_acquire))
			// ... nope, release time slice and keep trying
			std::this_thread::yield();
	}
	inline void unlock() { lock_.clear(std::memory_order_release); }

private:
	std::atomic_flag lock_ = ATOMIC_FLAG_INIT;
};

/*
	Implementation of Windows-style shared memory mapping
*/
class Mapping {
	HANDLE mH = nullptr;				// mapped obj handle
	LPBYTE vB = nullptr;				// mapped obj view base
	size_t vN = 0;						// mapped obj view size

public:
	Mapping() {}
	~Mapping ()
	{
		if (vN != 0)
			::UnmapViewOfFile(vB), ::CloseHandle(mH);
	}

	bool Create(LPCWSTR sharedObjName)
	{
		if (vN != 0)
			return true;	// (mapping ALREADY here)

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
		return true;
	}

	LPBYTE Base() const { return vB; }
	size_t Size() const { return vN; }
};

/*
	primary types for dealing with sensor paths and collections
*/
typedef wstring sensor_t;
typedef set<sensor_t> sensor_enumeration_t;

enum { Pages = 4, LayoutsPerPage = 8, BackgroundImages = 10 };

/*
	"universal" units to which all Monitor-specific units are mapped
*/
enum Unit {
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
	# of fractional digits to display for above "universal" units

	N.B. - Unit enums will be used as indices into this array, so
	make SURE they are kept in sync!
*/
static const int displayFractional[] {
	0,
	3, 0, 0, 3, 3, 1, 1,
	0, 3, 0, 1, 0, 0, 3,
	0, 0, 0,
	0
};

/*
	utility functions for accessing [sensor] path components
*/
static wstring head(const wstring& path)
{
	const auto i = path.find_first_of('|');
	return (i != wstring::npos) ? path.substr(0, i) : L"";
}

static wstring tail(const wstring& path)
{
	const auto i = path.find_last_of('|');
	return (i != wstring::npos) ? path.substr(i + 1) : L"";
}

static vector<wstring> split(const wstring& text, const wregex& sep)
{
	wsregex_token_iterator first(cbegin(text), cend(text), sep, -1), last;
	return { first, last };
}

/*
	primary INTERFACE for talking to [abstract] "Monitors"...
*/
class IMonitor {
public:
	virtual ~IMonitor() {};

	virtual wstring DisplayName() const = 0;
	virtual bool Refresh() = 0;
	virtual bool RefreshNeeded() const = 0;
	virtual const sensor_enumeration_t& Sensors() const = 0;
	virtual Unit SensorUnit(const wstring& path) const = 0;
	virtual float SensorValue(const wstring& path, bool fahrenheit = false) const = 0;
	virtual wstring SensorValueString(const wstring& path, bool fahrenheit = false) const = 0;
};

/*
	... common functions and data elements supporting IMonitor IMPLEMENTATIONS
*/
template<typename T>
class MonitorCommonImpl : public IMonitor {
protected:
	Mapping mapping;
	wstring root, displayName;
	sensor_enumeration_t sensors;
	map<sensor_t, T> values;
	map<sensor_t, Unit> units;
	RSpinLock lock;

	MonitorCommonImpl(wstring root, wstring displayName) : root(root), displayName(displayName) {}

	constexpr double c2f(double d) const { return floor((d * 9 / 5 + 32) + 0.5); }
	template<class SynchronizedInit>
	bool refreshImpl(SynchronizedInit f) {
		lock_guard<RSpinLock> acquire(lock);
		return f();
	}
	template<class RawValueAccess>
	float sensorValueImpl(const wstring& path, bool fahrenheit, RawValueAccess f) const {
		float ret = 0;
		lock_guard<RSpinLock> acquire(const_cast<RSpinLock&>(lock));
		const auto&& v = values.find(path);
		const auto&& u = units.find(path);
		if (v == cend(values) || u == cend(units))
			ret = numeric_limits<float>::infinity();	// sensor not present
		else try {
			double d = f(v->second, u->second);
			if (u->second == Degrees && fahrenheit)
				d = c2f(d);
			ret = (float)d;
		} catch (...) {
			ret = numeric_limits<float>::infinity();	// sensor not present
		}
		return ret;
	}

public:
	constexpr wstring DisplayName() const override {
		return displayName;
	}
	constexpr bool RefreshNeeded() const override { return false; }
	const sensor_enumeration_t& Sensors() const override {
		return sensors;
	}
	Unit SensorUnit(const wstring& path) const override {
		const auto&& u = units.find(path);
		return (u != cend(units)) ? u->second : None;
	}
	wstring SensorValueString(const wstring& path, bool fahrenheit) const override {
		wchar_t b[16];
		const auto u = SensorUnit(path);
		swprintf(b, 16, L"%.*f", displayFractional[u], SensorValue(path, fahrenheit));
		return b;
	}
};

/*
	Implementation of IMonitor for MSI Afterburner
*/
class ABMonitor : public MonitorCommonImpl<const float*>{

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

	typedef MAHM_SHARED_MEMORY_HEADER AbSharedMem;

	int enumerateSensors();
	AbSharedMem& ab() const { return *(AbSharedMem*)mapping.Base(); }
	MAHM_SHARED_MEMORY_ENTRY& rE(int i) const { return *(MAHM_SHARED_MEMORY_ENTRY*)(mapping.Base() + ab().dwHeaderSize + ab().dwEntrySize * i); }
	MAHM_SHARED_MEMORY_GPU_ENTRY& gE(int i) const { return *(MAHM_SHARED_MEMORY_GPU_ENTRY*)(mapping.Base() + ab().dwHeaderSize + ab().dwEntrySize * ab().dwNumEntries + ab().dwGpuEntrySize * i); }
	Unit unitFromRecord(const MAHM_SHARED_MEMORY_ENTRY& r) const;

public:
	ABMonitor(wstring root, wstring displayName) : MonitorCommonImpl(root, displayName) {}
	~ABMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create(L"MAHMSharedMemory") && enumerateSensors() > 0; });
	}
	float SensorValue(const wstring& path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) { return *i; });
	}
};

int ABMonitor::enumerateSensors()
{
	::OutputDebugString(L"ABMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	const auto& a = ab();

	if (a.dwSignature != 'MAHM')
		return 0; // nothing to see here...

	for (DWORD i = 0; i < a.dwNumEntries; ++i) {
		const auto& r = rE(i);
		const auto u = unitFromRecord(r);
		if (u != None) {
			wstringstream pathSS;
			pathSS << root << L'|';
			if (r.dwSrcId != 0xffffffff && r.dwSrcId < 0x80) {
				pathSS << gE(r.dwGpu).szDevice;
				/*
					append #2, #3 etc for any subsequent GPUs - a simplifying
					assumption, since cases when they AREN'T in a "standard"
					SLI/Crossfire configuration should be rare (we will see
					how the case of discrete GPU + integrated GPU stabilizes)
				*/
				if (r.dwGpu > 0)
					pathSS << L" #" << r.dwGpu + 1;
			} else
				pathSS << L"pc";
			pathSS << L'|' << r.szSrcName;
			wstring path(pathSS.str());
			sensors.insert(path);
			values[path] = &r.data, units[path] = u;
			::OutputDebugString(path.c_str());
		}
	}

	return sensors.size();
}

Unit ABMonitor::unitFromRecord(const MAHM_SHARED_MEMORY_ENTRY& r) const
{
	static const map<string, Unit> types {
		{ "V", Volts }, { "\xb0""C", Degrees }, { "RPM", RPM }, { "MHz", MHz },
		{ "%", UsagePerCent }, { "MB", MB }, { "FPS", FPS }, { "ms", MS }
	};
	const auto&& u = types.find(r.szSrcUnits);
	return u != cend(types) ? u->second : Unknown;
}

/*
	Implementation of IMonitor for GPU-Z
*/
class GPUZMonitor : public MonitorCommonImpl<const double*> {
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
	GpuzSharedMem& gpuz() const { return *(GpuzSharedMem*)mapping.Base(); }
	Unit unitFromRecord(const SensorRecord& r) const;

public:
	GPUZMonitor(wstring root, wstring displayName) : MonitorCommonImpl(root, displayName) {}
	~GPUZMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create(L"GPUZShMem") && enumerateSensors() > 0; });
	}
	float SensorValue(const wstring& path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) { return *i; });
	}
};

int GPUZMonitor::enumerateSensors()
{
	::OutputDebugString(L"GPUZMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	const auto& g = gpuz();

	const auto&& device = find_if(cbegin(g.data), cend(g.data), [](const Record& r) {
		return wcscmp(r.key, L"CardName") == 0;
	});
	wstring deviceName = (device != cend(g.data)) ? device->val : L"<Graphics Card>";

	for (const auto& s : g.sensors) {
		if (s.name[0] == L'\0')
			break;	// nothing more to examine
		const auto u = unitFromRecord(s);
		if (u != None) {
			wstringstream pathSS;
			pathSS << root << L'|' << deviceName << L'|' << s.name;
			wstring path(pathSS.str());
			sensors.insert(path);
			values[path] = &s.value, units[path] = u;
			::OutputDebugString(path.c_str());
		}
	}

	return sensors.size();
}

Unit GPUZMonitor::unitFromRecord(const SensorRecord& r) const
{
	static const map<wstring, Unit> types {
		{L"V", Volts}, {L"\u00b0C", Degrees}, {L"RPM", RPM}, {L"MHz", MHz},
		{L"%", UsagePerCent}, {L"%%", UsagePerCent}
	};
	const auto&& u = types.find(r.unit);
	return u != cend(types) ? u->second : None;
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

	const HWMDevice& device(int d) const { return ((HWMDevice*)(mapping.Base() + sizeof HWMHdr))[d]; }
	int deviceCount() const { return mapping.Base() ? ((HWMHdr*)mapping.Base())->deviceNum : 0; }
	const char* deviceDescription(int d) const { return (mapping.Base() && d < deviceCount()) ? device(d).description : "" ; }
	int enumerateSensors();
	const wchar_t* groupType(int g) const {
		static wchar_t* sensorGroupTypes[MaxGroups] {
			L"<voltages>",
			L"<temperatures>",
			L"<fans>",
			L"<PWM fans>",
			L"<currents>",
			L"<powers>",
			L"xxx", L"xxx", L"xxx", L"xxx"
		};
		return (g < MaxGroups) ? sensorGroupTypes[g] : L"";
	}
	const HWMSensor& node(int d, int g, int s) const { return ((HWMSensor*)(mapping.Base() + device(d).map[g].nodePtr))[s]; }
	int sensorCount(int d, int g) const { return (mapping.Base() && d < deviceCount() && g < MaxGroups) ? device(d).map[g].nodeNum : 0; }
	const char* sensorLabel(int d, int g, int s) const { return (mapping.Base() && d < deviceCount() && g < MaxGroups && s < sensorCount(d, g)) ? node(d, g, s).name : ""; }
	Unit unitFromDGS(int d, int g, int s) const;

public:
	HWMonitor(wstring root, wstring displayName) : MonitorCommonImpl(root, displayName) {}
	~HWMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create(L"$CPUID$HWM$") && enumerateSensors() > 0; });
	}
	float SensorValue(const wstring& path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) { return *i; });
	}
};

int HWMonitor::enumerateSensors()
{
	auto dgs = [](int d, int g, int s) {
		wchar_t buf[16];
		return
			wstring(_itow(d, buf, 10)) + L',' +
			wstring(_itow(g, buf, 10)) + L',' +
			wstring(_itow(s, buf, 10));
	};
	::OutputDebugString(L"HWMonitor::enumerateSensors...");
	multiset<wstring> devices;
	sensors.clear(), values.clear(), units.clear();

	for (int d = 0; d < deviceCount(); ++d) {
		string raw_dev(deviceDescription(d));
		wstring deviceName(cbegin(raw_dev), cend(raw_dev));
		devices.insert(deviceName);
		auto dupes = devices.count(deviceName);
		for (int g = 0; g < MaxGroups; ++g)
			for (int s = 0; s < sensorCount(d, g); ++s) {
				wstringstream pathSS;
				pathSS << root << L'|' << deviceName;
				if (dupes > 1)
					pathSS << " #" << dupes;
				// N.B. - ONLY CPUID Hardware Monitor needs this "extra" level
				pathSS << L'|' << groupType(g) << L'|' << sensorLabel(d, g, s);
				wstring path(pathSS.str());
				sensors.insert(path);
				values[path] = &node(d, g, s).value;
				units[path] = unitFromDGS(d, g, s);
				::OutputDebugString((dgs(d, g, s) + L'=' + path).c_str());
			}
	}

	return sensors.size();
}

Unit HWMonitor::unitFromDGS(int d, int g, int s) const {
	static Unit g2u[] { Volts, Degrees, RPM, RPM, Amps, Watts };
	return (mapping.Base() && d < deviceCount() && g < MaxGroups && s < sensorCount(d, g)) ? g2u[g] : None;
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

	const wchar_t* speedFanConfig = L"speedfansens.cfg";
	const wchar_t* speedFanExecutable = L"speedfan.exe";
	
	const wchar_t* cfgVersionTag = L"xxx version ";
	const int cfgVersionTagN = wcslen(cfgVersionTag);
	const wchar_t* cfgLinkTag = L"xxx Link UniqueID=";
	const int cfgLinkTagN = wcslen(cfgLinkTag);
	const wchar_t* cfgPwmTag = L"xxx Pwm ";
	const int cfgPwmTagN = wcslen(cfgPwmTag);
	const wchar_t* cfgSensorTag = L"xxx Sensor UniqueID=";
	const int cfgSensorTagN = wcslen(cfgSensorTag);
	const wchar_t* cfgDeviceNameTag = L"Name=";
	const int cfgDeviceNameTagN = wcslen(cfgDeviceNameTag);
	const wchar_t* cfgSensorNameTag = L"name=";
	const int cfgSensorNameTagN = wcslen(cfgSensorNameTag);
	const wchar_t* cfgTempTag = L"xxx Temp ";
	const int cfgTempTagN = wcslen(cfgTempTag);
	const wchar_t* cfgFanTag = L"xxx Fan ";
	const int cfgFanTagN = wcslen(cfgFanTag);
	const wchar_t* cfgVoltTag = L"xxx Volt ";
	const int cfgVoltTagN = wcslen(cfgVoltTag);
	const wchar_t* cfgEndTag = L"xxx end";
	const int cfgEndTagN = wcslen(cfgEndTag);

	int enumerateSensors();
	wstring SFMonitor::getExecutableDir(const wstring& exeToFind);
	SfSharedMem& sf() const { return *(SfSharedMem*)mapping.Base(); }

public:
	SFMonitor(wstring root, wstring displayName) : MonitorCommonImpl(root, displayName) {}
	~SFMonitor() override {}

	bool Refresh() override {
		return refreshImpl([this]() { return mapping.Create(L"SFSharedMemory_ALM") && enumerateSensors() > 0; });
	}
	float SensorValue(const wstring& path, bool fahrenheit = false) const override {
		return sensorValueImpl(path, fahrenheit, [](auto i, Unit u) {
			return u != RPM ? (double)*i / 100 : (double)*i;
		});
	}
};

int SFMonitor::enumerateSensors()
{
	::OutputDebugString(L"SFMonitor::enumerateSensors...");
	sensors.clear(), values.clear(), units.clear();
	wstring sfPath = getExecutableDir(speedFanExecutable);
	if (sfPath.empty())
		return 0;	// we're outta here
	wifstream config(sfPath + speedFanConfig);
	if (!config.good())
		return 0;	// we're outta here

	wstring version, longName, path;
	map<wstring, wstring> devices;
	path.reserve(128);
	int temps = 0, fans = 0, volts = 0;
	ParseState state = WantVer;
	wchar_t line[132];
	while (config.getline(line, 132).good()) {
		if (line[0] == L'\0')
			continue;

		switch (state) {
		case WantVer:
			if (wcsncmp(line, cfgVersionTag, cfgVersionTagN) == 0)
				version = line + cfgVersionTagN, state = WantTag;
			break;

		case WantTag:
			if (wcsncmp(line, cfgLinkTag, cfgLinkTagN) == 0 ||
				wcsncmp(line, cfgPwmTag, cfgPwmTagN) == 0)
				state = WantEnd;	// (ignore these and skip to end)
			else if (wcsncmp(line, cfgSensorTag, cfgSensorTagN) == 0)
				longName = line + cfgSensorTagN, state = WantDeviceName;
			else if (wcsncmp(line, cfgTempTag, cfgTempTagN) == 0)
				longName = wcsstr(line, L" from ") + 6, state = WantTempName;
			else if (wcsncmp(line, cfgFanTag, cfgFanTagN) == 0)
				longName = wcsstr(line, L" from ") + 6, state = WantFanName;
			else if (wcsncmp(line, cfgVoltTag, cfgVoltTagN) == 0)
				longName = wcsstr(line, L" from ") + 6, state = WantVoltName;
			break;

		case WantDeviceName:
			if (wcsncmp(line, cfgDeviceNameTag, cfgDeviceNameTagN) == 0)
				devices[longName] = line + cfgDeviceNameTagN, state = WantEnd;
			break;

		case WantTempName:
			if (wcsncmp(line, cfgSensorNameTag, cfgSensorNameTagN) == 0) {
				path = root + L'|', path += devices[longName], path += L"|<temperatures>|";
				path += line + cfgSensorNameTagN;
				values[path] = &sf().temps[temps++], units[path] = Degrees;
				sensors.insert(path);
				::OutputDebugString(path.c_str());
				state = WantEnd;
			}
			break;

		case WantFanName:
			if (wcsncmp(line, cfgSensorNameTag, cfgSensorNameTagN) == 0) {
				path = root + L'|', path += devices[longName], path += L"|<fans>|";
				path += line + cfgSensorNameTagN;
				values[path] = &sf().fans[fans++], units[path] = RPM;
				sensors.insert(path);
				::OutputDebugString(path.c_str());
				state = WantEnd;
			}
			break;

		case WantVoltName:
			if (wcsncmp(line, cfgSensorNameTag, cfgSensorNameTagN) == 0) {
				path = root + L'|', path += devices[longName], path += L"|<voltages>|";
				path += line + cfgSensorNameTagN;
				// UGLY HACK to deal with SpeedFan [config?] problem
				if (sensors.find(path) == cend(sensors)) {
					values[path] = &sf().volts[volts++], units[path] = Volts;
					sensors.insert(path);
					::OutputDebugString(path.c_str());
				} else
					::OutputDebugString((L"Rejected as dupe: " + path).c_str());
				state = WantEnd;
			}
			break;

		case WantEnd:
			if (wcsncmp(line, cfgEndTag, cfgEndTagN) == 0)
				state = WantTag;
			break;
		}
	}

	return sensors.size();
}

/*
	getExecutableDir(wstring exeToFind) - perform a case-INSENSITIVE search
	for the supplied exe in the running processes list, and return the full
	path of its FOLDER (including the trailing slash), or an empty string
*/
wstring SFMonitor::getExecutableDir(const wstring& exeToFind)
{
	// get all processes...
	HANDLE sH = ::CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
	if (sH == INVALID_HANDLE_VALUE)
		return L"";	// we're outta here

					// ... and see if SpeedFan is running
	wstring exePath;
	PROCESSENTRY32 pe { sizeof PROCESSENTRY32 };
	for (BOOL status = ::Process32First(sH, &pe); status; status = ::Process32Next(sH, &pe)) {
		wstring exe = pe.szExeFile;
		transform(cbegin(exe), cend(exe), begin(exe), ::tolower);
		if (exe.find(exeToFind) != wstring::npos) {
			wchar_t fullExe[MAX_PATH];
			DWORD nchars = MAX_PATH;
			HANDLE pH = ::OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pe.th32ProcessID);
			if (pH != nullptr) {
				if (::QueryFullProcessImageName(pH, 0, fullExe, &nchars))
					exePath = wstring(fullExe, nchars - exeToFind.length());
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
typedef map<wstring, unique_ptr<IMonitor>> wstring_monitor_map_t;
}

using namespace rxm;

// define our docklet instance data

static inline Color COLORREF2Color(COLORREF cr)
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
	HWND hwndDocklet;					// THIS docklet's HWND
	HINSTANCE hInstance;				// docklet's DLL HINSTANCE
	int page;							// current layout page
	int timer;							// Windows timer id
	int image;							// background selection
	int fahrenheit;						// 1=do Fahrenheit conversion
	wstring_monitor_map_t monitor;		// RXM Monitor-specific impls
	struct Layout {
		wstring path;					// our sensor
		wstring form;					// y-axis pos
		COLORREF rgb;					// this color
		float last;						// last value
		StringAlignment align;			// left|right

		bool Active() const { return !path.empty(); }
		void Assign(COLORREF c) { rgb = c; }
		void Assign(const wstring& p, COLORREF c) { path = p, rgb = c; }
		void Clear() { path.clear(), rgb = 0, last = -1; }
		void Create(const wstring& f, StringAlignment a) { form = f, rgb = 0, last = -1, align = a; }
		bool Live(RXM* rxm) const {
			const auto m = rxm->monitor.find(head(path));
			if (m == cend(rxm->monitor))
				return false;
			const auto& s = m->second.get()->Sensors();
			return s.empty() ? false : s.find(path) != cend(s);
		}
		void Render(RXM* rxm, Graphics& g, Gdiplus::Font& f, RectF& r, StringFormat& sf) {
			// display individual sensor with supplied GdiPlus formatting & attributes AS REQUIRED
			if (Active()) {
				wstring t = form, valString{ L'-' };
				if (Live(rxm)) {
					auto m = rxm->FromPath(path);
					if (m->RefreshNeeded())
						m->Refresh();
					last = m->SensorValue(path);
					valString = m->SensorValueString(path, rxm->fahrenheit == 1);
				}
				t.replace(t.find(L"%s"), 2, valString);
				sf.SetAlignment(align);
				g.DrawString(t.c_str(), -1, &f, r, &sf, &SolidBrush(COLORREF2Color(rgb)));
			}
		}
		bool UpdateRequired(RXM* rxm) const { return Active() && Live(rxm) && rxm->FromPath(path)->SensorValue(path) != last; }
	} layout[Pages][LayoutsPerPage];	// sensor layouts (all pages)

	IMonitor* FromPath(wstring path) const { return monitor.find(head(path))->second.get(); }
};

// RXMConfigure dialog interface

class RXMConfigure : public CDialogEx
{
	//DECLARE_DYNAMIC(RXMConfigure)

	RXM* rxm;							// OUR docklet instance data

	map<HTREEITEM, sensor_t> pathFromTree;
	map<sensor_t, HTREEITEM> treeFromPath;

	const int assignControlID[LayoutsPerPage] {
		IDC_ASSIGN1, IDC_ASSIGN2, IDC_ASSIGN3, IDC_ASSIGN4,
		IDC_ASSIGN5, IDC_ASSIGN6, IDC_ASSIGN7, IDC_ASSIGN8
	};
	const int colorControlID[LayoutsPerPage] {
		IDC_COLOR1, IDC_COLOR2, IDC_COLOR3, IDC_COLOR4,
		IDC_COLOR5, IDC_COLOR6, IDC_COLOR7, IDC_COLOR8
	};
	const int editControlID[LayoutsPerPage] {
		IDC_SENSOR1, IDC_SENSOR2, IDC_SENSOR3, IDC_SENSOR4,
		IDC_SENSOR5, IDC_SENSOR6, IDC_SENSOR7, IDC_SENSOR8
	};
	const int locateControlID[LayoutsPerPage] {
		IDC_LOCATE1, IDC_LOCATE2, IDC_LOCATE3, IDC_LOCATE4,
		IDC_LOCATE5, IDC_LOCATE6, IDC_LOCATE7, IDC_LOCATE8
	};

	void assignSensor(int sensor);
	void assignColor(int sensor);
	void initializeBackgroundList();
	void initializeSensors();
	void initializeSensorTree();
	void locateSensor(int sensor);
	void unassignSensor(int sensor);

public:
	RXMConfigure(RXM* rxm, CWnd* pParent = nullptr);   // standard constructor
	virtual ~RXMConfigure();

// Dialog Data
	enum { IDD = IDD_RXM_CONFIGURE };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

public:
	CTabCtrl SensorTab;
	CTreeCtrl SensorTree;
	CComboBox Background;
	int CelsiusOrFahrenheit;
	CMFCColorButton fake;				// (apparently needed to drag in some code/resource?)
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

static void initializeRXM(RXM* rxm, HWND hwndDocklet, HINSTANCE hInstance)
{
	rxm->hwndDocklet = hwndDocklet, rxm->hInstance = hInstance;
	rxm->page = 0, rxm->timer = 0, rxm->image = 0, rxm->fahrenheit = 0;

	static const wstring layoutStrings[LayoutsPerPage / 2] { L"%s\n\n\n\n", L"\n%s\n\n\n", L"\n\n%s\n\n", L"\n\n\n%s\n" };

	for (auto& p : rxm->layout)
		for (int s = 0; s < LayoutsPerPage; ++s)
			// set location-based formatting (only 4 formats, used both on left and right)
			p[s].Create(layoutStrings[s & 0x03], (s < 4) ? StringAlignmentNear : StringAlignmentFar);
}

static void loadProfile(RXM* rxm, const string& ini, const string& iniGroup)
{
	wstring iniW(cbegin(ini), cend(ini)), iniGroupW(cbegin(iniGroup), cend(iniGroup));
	// slurp in sensor, color, and temperature settings
	for (int p = 0; p < Pages; ++p)
		for (int s = 0; s < LayoutsPerPage; ++s) {
			wchar_t k1[16], k2[16], b[MAX_PATH];
			swprintf(k1, 16, L"Sensor%d-%d", p + 1, s + 1);
			if (::GetPrivateProfileString(iniGroupW.c_str(), k1, L"", b, MAX_PATH, iniW.c_str())) {
				swprintf(k2, 16, L"Color%d-%d", p + 1, s + 1);
				rxm->layout[p][s].Assign(b, ::GetPrivateProfileInt(iniGroupW.c_str(), k2, 0, iniW.c_str()));
			}
		}

	const int j = ::GetPrivateProfileInt(iniGroupW.c_str(), L"Fahrenheit", 0, iniW.c_str());
	rxm->fahrenheit = (j == 0)? 0 : 1;
	const int k = ::GetPrivateProfileInt(iniGroupW.c_str(), L"Background", 0, iniW.c_str());
	rxm->image = __min(__max(k, 0), BackgroundImages-1);
}

static bool pageIsActive(RXM* rxm)
{
	return any_of(cbegin(rxm->layout[rxm->page]), cend(rxm->layout[rxm->page]), [](auto l) {
		return l.Active();
	});
}

static void renderBackground(RXM* rxm)
{
	auto bm = make_unique<Bitmap>(128, 128, PixelFormat32bppARGB);
	Graphics g(bm.get());
	auto drawGrid = [&](Color argb) {
		Pen p(argb);
		for (int x = 16; x < 128; x += 16)
			g.DrawLine(&p, x - 0.5f, -0.5f, x - 0.5f, 127.5f);
		for (int y = 16; y < 128; y += 16)
			g.DrawLine(&p, -0.5f, y - 0.5f, 127.5f, y - 0.5f);
	};
	auto drawRect = [&](Color argb) {
		Pen p(argb);
		g.DrawRectangle(&p, 0, 0, 127, 127);
	};
	auto fillRect = [&](Color argb) {
		SolidBrush b(argb);
		g.FillRectangle(&b, 0, 0, 128, 128);
	};
	static const Color Black = Color(255, 0, 0, 0);
	static const Color Clear = Color(0, 0, 0, 0);
	static const Color SoftBlue = Color(128, 0, 128, 255);
	static const Color SoftGray = Color(128, 0, 0, 0);
	static const Color White = Color(255, 255, 255, 255);
	static const Color Yellow = Color(255, 250, 242, 173);

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

	DockletSetImage(rxm->hwndDocklet, bm.release());
}

static void renderPage(RXM* rxm, bool force = false)
{
	// display all sensors on current page AS REQUIRED
	if (force || any_of(cbegin(rxm->layout[rxm->page]), cend(rxm->layout[rxm->page]), [rxm](const RXM::Layout& l) {
		return l.UpdateRequired(rxm);
		})) {
		// yup, update *is* required
		auto bm = make_unique<Bitmap>(128, 128, PixelFormat32bppARGB);
		Graphics g(bm.get());
		g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
		StringFormat sf(0, LANG_NEUTRAL);
		sf.SetLineAlignment(StringAlignmentCenter);
		static RectF r(0, 0, 128, 128);
		Gdiplus::Font f(L"Arial", 15);

		// FOREACH [in-use] layout on current page...
		for (auto& l : rxm->layout[rxm->page])
			l.Render(rxm, g, f, r, sf);

		DockletSetImageOverlay(rxm->hwndDocklet, bm.release());
	}
}

static void saveProfile(RXM* rxm, const string& ini, const string& iniGroup, bool asDefault = false)
{
	wstring iniW(cbegin(ini), cend(ini)), iniGroupW(cbegin(iniGroup), cend(iniGroup));
	// handle "save local defaults" AS REQUIRED
	if (asDefault)
		WritePrivateProfileInt(iniGroupW.c_str(), L"ForceDockletDefaults", 1, iniW.c_str());
	// stash sensor, color, and temperature settings
	for (int p = 0; p < Pages; ++p)
		for (int s = 0; s < LayoutsPerPage; ++s) {
			RXM::Layout& l = rxm->layout[p][s];
			wchar_t k1[16], k2[16];
			swprintf(k1, 16, L"Sensor%d-%d", p + 1, s + 1);
			swprintf(k2, 16, L"Color%d-%d", p + 1, s + 1);
			if (l.Active()) {
				::WritePrivateProfileString(iniGroupW.c_str(), k1, l.path.c_str(), iniW.c_str());
				WritePrivateProfileInt(iniGroupW.c_str(), k2, l.rgb, iniW.c_str());
			} else {
				::WritePrivateProfileString(iniGroupW.c_str(), k1, nullptr, iniW.c_str());
				::WritePrivateProfileString(iniGroupW.c_str(), k2, nullptr, iniW.c_str());
			}
		}
	WritePrivateProfileInt(iniGroupW.c_str(), L"Fahrenheit", rxm->fahrenheit, iniW.c_str());
	WritePrivateProfileInt(iniGroupW.c_str(), L"Background", rxm->image, iniW.c_str());
}

// ObjectDock SDK 1.0 callbacks

RXM* CALLBACK OnCreateRXM(HWND hwndDocklet, HINSTANCE hInstance, char *szIni, char *szIniGroup)
{
	::OutputDebugString(L"OnCreateRXM...");
	auto rxm = make_unique<RXM>();
	initializeRXM(rxm.get(), hwndDocklet, hInstance);

	// create the specialized Monitors...
	rxm->monitor.emplace(L"ABM", make_unique<ABMonitor>(L"ABM", L"AfterBurner"));
	rxm->monitor.emplace(L"GPUZ", make_unique<GPUZMonitor>(L"GPUZ", L"GPU-Z"));
	rxm->monitor.emplace(L"HWM", make_unique<HWMonitor>(L"HWM", L"HWMonitor"));
	rxm->monitor.emplace(L"SF", make_unique<SFMonitor>(L"SF", L"SpeedFan"));
	// ... and initialize the ones that are [initially] present
	for (auto& m : rxm->monitor)
		m.second->Refresh();

	if (rxm->monitor.empty())
		DockletSetLabel(hwndDocklet, "Start a Monitor application!");

	// load profile (if there is one)...
	string ini(szIni ? szIni : ""), iniGroup(szIniGroup ? szIniGroup : "");
	if (!ini.empty() && !iniGroup.empty())
		loadProfile(rxm.get(), ini, szIniGroup);
	else if (!rxm->monitor.empty())
		DockletSetLabel(hwndDocklet, "Configure Docklet!");
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
	::OutputDebugString(L"OnDestroyRXM...");
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
		"MSI Afterburner (gaming.msi.com/features/afterburner),\r\n"
		"SpeedFan (www.almico.com/speedfan.php),\r\n"
		"and CPUID HWMonitor (www.cpuid.com).\r\n"
		"\r\n"
		"Copyright (C) 2009-2017 Robert Roessler\r\n"
		"www.rftp.com");
}

BOOL CALLBACK OnLeftButtonClick(RXM* rxm, POINT *ptCursor, SIZE *sizeDocklet)
{
	// show "next" page of sensors (N.B. - "Pages" must be a power of 2)
	const int delta = (::GetKeyState(VK_SHIFT) < 0) ? -1 : 1;
	const int i = rxm->page;
	do
		rxm->page = (rxm->page + delta) & (Pages - 1);
	while (!pageIsActive(rxm) && rxm->page != i);
	renderPage(rxm, rxm->page != i);
	return TRUE;
}

void CALLBACK OnProcessMessage(RXM* rxm, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	// process message... like, say, WM_TIMER
	switch (uMsg) {
	// WM_TIMER: update sensor display(s) AS REQUIRED
	case WM_TIMER:
		if (wParam == 43)
			renderPage(rxm);
		break;

	// WM_POWERBROADCAST: on resume, re-enumerate sensors AS REQUIRED
	case WM_POWERBROADCAST:
		if (wParam == PBT_APMRESUMEAUTOMATIC)
			for (auto& m : rxm->monitor)
				m.second->Refresh();
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
	m.AppendMenu(MF_STRING|MF_ENABLED, 1, L"Configure...");
	m.AppendMenu(MF_STRING|MF_ENABLED, 2, L"Refresh");
	m.AppendMenu(MF_STRING|MF_DISABLED, 3, L"About RXM Docklet");
	m.AppendMenu(MF_SEPARATOR);
	m.AppendMenu(MF_STRING|MF_ENABLED, 4, L"Dock Menu");
	BOOL rv = FALSE;

	DockletLockMouseEffect(rxm->hwndDocklet, TRUE);
	const int i = ::TrackPopupMenu(m, TPM_RETURNCMD|TPM_NONOTIFY|TPM_RIGHTBUTTON|TPM_LEFTALIGN|TPM_TOPALIGN,
		p.x, p.y, 0, rxm->hwndDocklet, nullptr);
	// so, what to do...
	switch (i) {
	case 1: {
		// configure docklet
		RXMConfigure cfg(rxm);
		const int oldPage = rxm->page, oldFahrenheit = rxm->fahrenheit, oldImage = rxm->image;
		decltype(rxm->layout) oldLayout;
		for (int p = 0; p < Pages; ++p)
			for (int s = 0; s < LayoutsPerPage; ++s)
				oldLayout[p][s] = rxm->layout[p][s];
		if (cfg.DoModal() != IDOK) {
			// user changed their mind, put it all back... AS REQUIRED
			for (int p = 0; p < Pages; ++p)
				for (int s = 0; s < LayoutsPerPage; ++s)
					rxm->layout[p][s] = oldLayout[p][s];
			// ... and try to save a little [resource allocation] work
			if (rxm->image != oldImage) {
				rxm->image = oldImage;
				renderBackground(rxm);
			}
			rxm->page = oldPage, rxm->fahrenheit = oldFahrenheit;
			renderPage(rxm, true);
		}
		rv = TRUE;
		break;
	}
	case 2:
		// "refresh" (actually, re-enumerate) Monitors
		for (auto& m : rxm->monitor)
			m.second->Refresh();
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

RXMConfigure::RXMConfigure(RXM* rxm, CWnd* pParent /*=nullptr*/)
	: CDialogEx(RXMConfigure::IDD, pParent)
	, rxm(rxm)
	, CelsiusOrFahrenheit(0)
{
}

RXMConfigure::~RXMConfigure()
{
}

void RXMConfigure::DoDataExchange(CDataExchange* pDX)
{
	CDialogEx::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_SENSOR_TAB, SensorTab);
	DDX_Control(pDX, IDC_SENSOR_TREE, SensorTree);
	DDX_Control(pDX, IDC_BACKGROUND, Background);
	DDX_Radio(pDX, IDC_CELSIUS, CelsiusOrFahrenheit);
	DDX_Control(pDX, IDC_COLOR1, fake);
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
	RXM::Layout& l = rxm->layout[rxm->page][sensor];
	if (!l.Active())
		return;	// nothing to do

	l.Assign(((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->GetColor());
	renderPage(rxm, true);
}

void RXMConfigure::assignSensor(int sensor)
{
	// set this sensor's corresponding hardware sensor...
	HTREEITEM h = SensorTree.GetSelectedItem();
	if (h == nullptr || SensorTree.ItemHasChildren(h))
		return;	// we're outta here

	// ... from the displayed sensor tree
	wstring path = pathFromTree[h];
	wstring elidedPath = head(path) + L'\u2026' + tail(path);
	((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText(elidedPath.c_str());
	rxm->layout[rxm->page][sensor].Assign(path, ((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->GetColor());
	renderPage(rxm);
}

void RXMConfigure::initializeBackgroundList()
{
	static wchar_t* b[BackgroundImages] {
		L"Black",
		L"Clear", L"Clear with Border", L"Clear with Grid",
		L"White", L"White with Border", L"White with Grid",
		L"Yellow", L"Yellow with Border", L"Yellow with Grid"
	};
	CComboBox* cb = (CComboBox*)GetDlgItem(IDC_BACKGROUND);
	cb->SetRedraw(FALSE);
	for (const auto& t : b)
		cb->AddString(t);
	cb->SetRedraw(TRUE);
}

void RXMConfigure::initializeSensors()
{
	for (auto& l : rxm->layout[rxm->page]) {
		const auto sensor = distance(rxm->layout[rxm->page], &l);
		if (l.Active()) {
			// display this sensor's color...
			((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->SetColor(l.rgb);
			// ... and [somewhat descriptive] name
			wstring elidedPath = head(l.path) + L'\u2026' + tail(l.path);
			((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText(elidedPath.c_str());
		} else {
			// (nothing [much] to do)
			((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->SetColor(RGB(192, 192, 192));
			((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText(L"");
		}
	}
}

void RXMConfigure::initializeSensorTree()
{
	static const wregex pathSeparator(L"\\|");

	// FOREACH specialized Monitor...
	for (const auto& pair : rxm->monitor) {
		const auto& sensors = pair.second.get()->Sensors();
		if (sensors.empty())
			continue;
		const int n = split(*cbegin(sensors), pathSeparator).size();
		vector<wstring> treeName(n, L"");
		vector<HTREEITEM> treeHandle(n, nullptr);
		// ... build the sensor tree from the [ordered] set of paths
		for (const auto& s : sensors) {
			auto path = split(s, pathSeparator);
			for (int i = 0; i < n; ++i)
				if (path[i] != treeName[i])
					if (i == 0)
						treeHandle[0] = SensorTree.InsertItem(pair.second.get()->DisplayName().c_str(), TVI_ROOT),
						treeName[0] = path[0];
					else
						treeHandle[i] = SensorTree.InsertItem(path[i].c_str(), treeHandle[i - 1]),
						treeName[i] = path[i], fill(begin(treeName) + i + 1, end(treeName), L"");

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
	RXM::Layout& l = rxm->layout[rxm->page][sensor];
	if (!l.Active() || !l.Live(rxm))
		return;	// we're outta here

	// ... from the assigned sensor path (also set focus)
	const HTREEITEM h = treeFromPath[l.path];
	SensorTree.SetFocus();
	SensorTree.EnsureVisible(h);
	SensorTree.SelectItem(h);
}

void RXMConfigure::unassignSensor(int sensor)
{
	// "free" (unassign) this sensor
	((CMFCColorButton*)GetDlgItem(colorControlID[sensor]))->SetColor(RGB(192, 192, 192));
	((CEdit*)GetDlgItem(editControlID[sensor]))->SetWindowText(L"");
	rxm->layout[rxm->page][sensor].Clear();
	renderPage(rxm, true);
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
	rxm->fahrenheit = CelsiusOrFahrenheit = 0;
	renderPage(rxm, true);
}

void RXMConfigure::OnBnClickedFahrenheit()
{
	rxm->fahrenheit = CelsiusOrFahrenheit = 1;
	renderPage(rxm, true);
}

void RXMConfigure::OnBnClickedSavelocal()
{
	char pathN[MAX_PATH];
	DockletGetRelativeFolder(rxm->hwndDocklet, pathN);
	string path(pathN);
	saveProfile(rxm, "Docklets\\Defaults.ini", path + "RXMDocklet.dll", true);
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
	SensorTab.InsertItem(0, L"Page 1");
	SensorTab.InsertItem(1, L"Page 2");
	SensorTab.InsertItem(2, L"Page 3");
	SensorTab.InsertItem(3, L"Page 4");
	SensorTab.SetCurSel(rxm->page);

	// init Sensor, Temperature, and Background controls
	initializeSensors();
	initializeBackgroundList();
	CelsiusOrFahrenheit = rxm->fahrenheit;
	Background.SetCurSel(rxm->image);

	UpdateData(FALSE);

	// "locate" 1st in-use sensor in tree
	BOOL ret = TRUE;
	const auto l = find_if(cbegin(rxm->layout[0]), cend(rxm->layout[0]), [](auto l) { return l.Active(); });
	if (l != cend(rxm->layout[0]))
		locateSensor(l - cbegin(rxm->layout[0])), ret = FALSE;

	return ret;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void RXMConfigure::OnTcnSelchangeSensorTab(NMHDR *pNMHDR, LRESULT *pResult)
{
	rxm->page = SensorTab.GetCurSel();
	initializeSensors();
	renderPage(rxm, true);
	*pResult = 0;
}

void RXMConfigure::OnTvnGetInfoTipSensorTree(NMHDR *pNMHDR, LRESULT *pResult)
{
	LPNMTVGETINFOTIP pGetInfoTip = reinterpret_cast<LPNMTVGETINFOTIP>(pNMHDR);
	HTREEITEM h = pGetInfoTip->hItem;
	// (make sure we are dealing with a LEAF)
	if (!SensorTree.ItemHasChildren(h)) {
		const wstring& path = pathFromTree[h];
		const auto m = rxm->FromPath(path);
		const wstring s = m->SensorValueString(path, CelsiusOrFahrenheit == 1);
		const rxm::Unit u = m->SensorUnit(path);
		/*
			display representation for above "universal" units

			N.B. - Unit enums will be used as indices into this array, so
			make SURE they are kept in sync!
		*/
		static const wchar_t* unitString[] {
			L"None",
			L"V", L"\u00b0", L"rpm", L"A", L"W", L"MHz", L"%",
			L"MB", L"MB/s", L"", L"GT/s", L"T", L"x", L"KB/s",
			L"F/s", L"ms", L"GB",
			L"???"
		};
		wstringstream tipSS;
		tipSS << s;
		if (unitString[u][0])
			tipSS << L' ' << unitString[u];
		if (u == Degrees)
			tipSS << ((CelsiusOrFahrenheit == 1) ? L'F' : L'C');
		wcsncpy(pGetInfoTip->pszText, tipSS.str().c_str(), pGetInfoTip->cchTextMax);
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
