# RXMDocklet

[![Build status](https://ci.appveyor.com/api/projects/status/github/robertroessler/rxmdocklet?svg=true)](https://ci.appveyor.com/project/robertroessler/rxmdocklet)

The RXMDocklet project grew out of an earlier work, "MBMDocklet", which only
worked with the "Motherboard Monitor" app by Alex Van Kaam, popular from the
late 90s through about 2004.

After this, RXMDocklet was created by abstracting the internal models and data
structures used so that potentially *any* hardware "monitor" type app (or apps)
[that makes its readings available in a shared memory area]
could be used as sources of data to display in the ObjectDock docklet.

RXMDocklet is quite simple in its primary interface, displaying from 1 to 8
selected readings (each in a custom color) from the available sources over a
choice of backgrounds - but always on top of the docklet's icon in the dock.

The "Configuration" dialog presents an intuitive graphical display allowing
both the setting of particular display "slots" from a tree representation of
the available sources and sensors (the "<<" button), as well as queries to
determine exactly which element in the sensor tree is the source of data for a
particular display slot (the ">>" button).  To completely *unassign* any single
display slot, just select the text of its path and delete it.

Finally, since 8 display slots may not be adequate for your use cases, either in
terms of total readings presented, or desired logical grouping, RXMDocklet is
able to choose from 1-4 "pages" to be displayed at any given time - just left-click
on the docklet itself to select the "next" page.

It is written in fairly idiomatic modern C++ 17/20, as well as using the MFC
application framework - it is this last that causes the unfortunately large size
of the DLL, since there has been quite a lot of "bloat" in MFC executables over the
years, especially if one uses some of the expanded set of available controls.

Besides being "pure" C++, the code is believed to be both 32/64 -bit "safe", BUT
as the only dock(s) it has been tried with are 32-bit apps, RXMDocklet has only
received a workout in 32-bit form.

## Supported Monitoring Apps

* GPU-Z
* HWiNFO (1)
* MSI Afterburner
* Core Temp
* SpeedFan
* CPUID HWMonitor (2)

1) with limitations in non-licensed versions, see comments at end of More Details
2) only versions 1.14-1.16, again, see comments at end of More Details

## Quickstart

Usage of RXMDocklet is simple:

*	copy the DLL (either a pre-built one from a RXMDocklet release, or one you
	have built yourself) to the "Docklets" folder under your ObjectDock or other
	chosen dock installation (an appropriately named subfolder under this also
	works), and click on your dock's "Add Item" menu entry - that's it!

*	left-click *on the docklet itself* will cycle through any pages that have
	sensor values defined (**holding down the SHIFT key** will cycle in reverse),
    while right-click will provide access to the Refresh and Configuration
    functionality
 
*	besides establishing the sensor mappings themselves, the available options
	include selecting the docklet backdrop image to display the values over,
	saving the current mappings between sensors and pages / display slots on the
	docklet, and the ever-popular choice of Celsius or Fahrenheit display of
	temperature data

## Advanced Features and Usage

*   starting with v1.3, RXMDocklet now will automatically detect 1- and 2-
    sensor pages, switching to a larger font for 2-sensor pages, and for 1-sensor
    pages, use that same larger font, but with the value on the top, and a
    descriptive "units" string on the bottom

*   clicking on individual sensors in the normal docklet display - **with the ALT key
    held down** - will *temporarily* switch to the 1-sensor enhanced view for that
    sensor for about 5 seconds, and then revert to the normal view of that page's
    sensors... this referred to as "focusing" or "zooming"

*   also with v1.3, RXMDocklet will now *dynamically adjust* the displayed precision
    of numeric values to make the best use of docklet real estate... within limits

## More Details

Remember that RXMDocklet is dependent on the use of monitoring apps to do the
actual collection/generation of data values to be displayed by RXMDocklet... so
do make sure to actually have one or more of these running *before* you start
your chosen dock, as that is when RXMDocklet is in turn started, and looks to
see which of these primary monitoring apps are available.  If you *do* wish to
start one or more additional monitoring apps *after* RXMDocklet is running, no
problem - just select Refresh from the docklet context menu.

When performing any "configuration" of the monitoring apps, it is suggested
that you instruct them to display any temperature info as "Celsius"... this is
because RXMDocklet itself will attempt to do the Celsius-to-Fahrenheit convert
operation for you, and we would probably rather avoid the confusion of doing
this twice.

Other than mentioning one more "gotcha" if you are using MSI Afterburner as a
data source for RXMDocklet (GPU voltage monitoring must be explicitly enabled on
both the General and Monitoring pages of that app's setup), you are pretty much
expected to take care of the setup and operation of the primary monitoring apps
yourself.

When using the RXMDocklet "Configuration" dialog, keep these things in mind:

*	each of the 1-8 "display slots" on a page has its own sensor assignment (if
	any), as well as its own color

*	given the somewhat constrained display area, it pays to give some thought to
	what is being displayed in adjacent columns, e.g., voltages are the widest
	values that are displayed (4 digits and a decimal point), so "pairing" with
	something shorter like a temperature (2-3 digits) is a good choice

*	along with the above advice on the manual laying-out of your docklet pages,
	note that you are not required to "fill in" any or all of a page - you could
	choose to fill in only the four corner positions, for instance

The key "dependency" which determines whether RXMDocklet can work with any
particular monitoring app is whether that app makes its sensor data visible
in a "shared memory" segment - and even then, it definitely helps if the app
authors make the details of their layout/usage of this shared memory public.

There are cases where the author of a given monitoring app may be basing a
commercial product on their app's ability to collect complete and accurate
sensor readings, creating a potential "clash" between openly sharing the
results of their substantial work... and not.  This results in some different
limitations (and workarounds).

In the case of the excellent HWiNFO, there is now differing behavior between
licensed and non-licensed versions... with the former, all is good, while with
the latter, the shared memory segment containing the HWiNFO readings that
RXMDocklet uses "expires" after 12 hours (starting with version 7 onwards).

To work around this, either purchase the licensed version of HWiNFO, or every
12 hours, the displayed values in RXMDocklet will either "freeze" or display
a "-" (indicating invalid data reads).  In either case, bring up HWiNFO's
Settings page and re-enable "Shared Memory Support"... you *may* need to also
use RXMDocklet's "Refresh" function.

As for CPUID HWMonitor, their shared memory access went "closed" over 10 years
ago now... after version "1.16".

## Credits

In addition to the enormous amount of work from all of the authors of the system
monitoring tools themselves, RXMDocklet builds on the foundation contributed by
Stardock: ObjectDock itself, which has made the "docklet" ecosystem possible.

For those wishing to develop docklets - or just understand their structure and
operation - Stardock makes available the "ObjectDock Docklet SDK v1.0", from

http://storage.stardock.com/files/ObjectDock_Docklet_SDK_v1.0.zip

NOTE that there is a minor bug in the contained file DockletSDK/DockletSDK.cpp:

The definition of the final function in the file (WritePrivateProfileInt) will
not compile under the Microsoft compiler's "Unicode Character Set" setting.  The
RXMDocklet repo includes a "fixed" version of this file, which can be found with
the accompanying DockletSDK.h in the "sdk" folder (the latter has NO changes).

For those who prefer patches, here it is:

    188a189,192
    > static inline LPSTR  itoX(int i, LPSTR  sz, int r) { return _itoa(i, sz, r); }
    > 
    > static inline LPWSTR itoX(int i, LPWSTR sz, int r) { return _itow(i, sz, r); }
    > 
    193,196c197,198
    < 	char szNumber[100];
    < 	strcpy(szNumber, "");
    < 	itoa(iValue, szNumber, 10);
    < 	return WritePrivateProfileString(lpAppName, lpKeyName, szNumber, lpFileName);
    ---
    > 	TCHAR szNumber[24];
    > 	return WritePrivateProfileString(lpAppName, lpKeyName, itoX(iValue, szNumber, 10), lpFileName);

## ToDo

Possible items to work on - for myself or collaborators include

* there will always be additional monitoring apps that can be supported inside
RXMDocklet - it takes only a few hours (or less) to add a new one

* although this may end up under **Probably Not** below, a more "universal" solution
to dynamically adjusting displayed precision of numeric values would include actually
selecting different units based on the magnitude of the value, *e.g.*, MB -> GB -> TB
... RXMDocklet currently works exclusively with the units selected by the monitoring
apps themselves

* investigate RXMDocklet on 64-bit "docks" - not that this is NOT the same as
whether your system itself is 32 or 64 -bit - it seems that many if not all of
the available dock apps themselves are 32-bit, which of course [should] work on
both 32/64-bit Windows installations

## ProbablyNot

Things that most likely will NOT happen include

* let's see - MAYBE allowing the choice of fonts used on the docklet?  The one
used in RXMDocklet ("Arial") is a compromise between size and readibility...
**UPDATE** - as of v1.3, RXMdocklet *will* use a larger font in some automatically
detected situations (see **Advanced Features and Usage** above)
