# On Airchive
[![License](http://poser.pugx.org/tzabzlat/yii2-sentry/license)](https://packagist.org/packages/tzabzlat/yii2-sentry)

A daemon for background recording of internet radio. Runs in the background, creates hourly audio files, and saves program titles.

*Read this in other languages: [English](README.md), [Русский](README.ru.md)*

---

### Key Features

- **Auto-recovery** - automatic reconnection on network failures
- **Parallel recording** - ability to record multiple radio stations simultaneously
- **Silence detection** - reconnection when silence is detected in the stream
- Can be installed as a Windows service for auto-start without user interaction
- Configurable RAM buffering to minimize disk load
- AAC encoding via ffmpeg
- Automatic timestamped file creation
- File rotation by date and time
- Support for different timezones for each stream
- Event logging

---

## How to Use

### Windows GUI

A graphical application for Windows with a visual interface.

**Launch:**
1. Download the Windows build zip archive
2. Extract to a convenient folder
3. Edit `config.ini` - specify the URLs of your streams
4. Run `OnAirchiveVCL.exe`
5. Monitor the recording in the application window

**Windows build includes:**
- BASS libraries in `Libs/Windows/x86/` (bass.dll, bassenc.dll, bass_aac.dll)
- Pre-compiled ffmpeg.exe (32-bit) from [sudo-nautilus](https://github.com/sudo-nautilus/FFmpeg-Builds-Win32/releases/download/latest/ffmpeg-n5.1-latest-win32-gpl-5.1.zip)

### Windows Service

A background service for continuous operation on Windows servers.

**Service installation and startup:**

Run console as administrator:

```cmd
; Install the service
OnAirchiveSVC.exe /install

:: Start the service
net start OnAirchiveSVCService
```

**Management via GUI:**

1. Press `Win + R`
2. Type `services.msc` and press Enter
3. Find **OnAirchiveSVCService** in the services list
4. Manage the service through the context menu (Start/Stop/Restart)

**Additional commands:**

```cmd
; Stop
net stop OnAirchiveSVCService

; Uninstall the service
OnAirchiveSVC.exe /uninstall
```

### Linux Console

A console application for Linux servers.

**Step 1: Install ffmpeg package**

```bash
apt-get install ffmpeg
```

**Step 2: Run via script**

```bash
./run.sh
```

**Important notes for Linux:**
- BASS libraries for Linux are included in the repository at `Libs/Linux/x86_64/` (libbass.so, libbassenc.so, libbass_aac.so)
- Libraries must be in the same folder as the executable

**Example configuration (config.ini):**

```ini
; General settings
[General]
; RAM buffer size to reduce disk access frequency
DataBufferSizeKb=5120

; Root directory for recordings
[Storage]
FilesPath=MyRadioArchive
; FilesPath=D:\MyRadioArchive
; FilesPath=/home/user/archive

; Radio station stream settings
[Stream]
; Radio station stream URL in mp3 or aac format
StreamUri0=https://am.radioaurora.am/al.mp3
; Subfolder with this name will be created in FilesPath directory
StreamToken0=Aurora
; Radio station timezone for correct timestamps
StreamTz0=Asia/Yerevan
```

---

## Output

The program automatically organizes recordings by hour:

```
root/ (FilesPath)
└── Aurora/
    └── 2020-11-23/
        ├── [2020-11-23 00-00-00] Aurora.aac
        ├── [2020-11-23 00-00-00] Aurora.txt
        ├── [2020-11-23 01-00-00] Aurora.aac
        ├── [2020-11-23 01-00-00] Aurora.txt
        ├── [2020-11-23 02-00-00] Aurora.aac
        ├── [2020-11-23 02-00-00] Aurora.txt
        └── ...
```

**File formats:**
- **`*.aac`** - audio file for one hour (in StreamTz timezone)
- **`*.txt`** - list of programs/tracks for the current hour (if station transmitted tags)

## How to Build

### Supported Platforms and Tools

| Platform | Build Target | Tool | Output File |
|----------|--------------|------|-------------|
| **Windows** | GUI (Desktop) | Delphi 10.2+ / RAD Studio | OnAirchiveVCL.exe |
| **Windows** | Service | Delphi 10.2+ / RAD Studio | OnAirchiveSVC.exe |
| **Linux** | Console (Daemon) | Free Pascal 3.2+ | onairchive |

### Building for Windows

#### Requirements:
- **Embarcadero Delphi** 10.2 Tokyo or higher (or RAD Studio)
- **Windows** 7 SP1 or higher
- BASS libraries are already included in the repository

#### Building via Delphi IDE:

**Windows GUI:**
1. Open `OnAirchive.groupproj` in RAD Studio
2. Select **Project → Build OnAirchiveVCL**
3. Select **Release** configuration
4. Result: `Output/Windows/Release/OnAirchiveVCL.exe`

**Windows Service:**
1. Open `OnAirchive.groupproj` in RAD Studio
2. Select **Project → Build OnAirchiveSVC**
3. Select **Release** configuration
4. Result: `Output/Windows/Release/OnAirchiveSVC.exe`

### Building for Linux

#### Requirements:
- **Free Pascal Compiler (FPC)** 3.2.0 or higher
- **ffmpeg** (installed via package manager)
- BASS libraries for Linux (already included in the repository)

#### Installing dependencies:

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install fpc ffmpeg
```

#### Building:

**Option 1: Build script (recommended)**

```bash
cd Platforms/Linux/
chmod +x build.sh
./build.sh
```

Result: `Output/Linux/onairchive`
Run via `./run.sh`

**Note:** The repository already includes BASS libraries for Linux in `Libs/Linux/x86_64/` (libbass.so, libbassenc.so, libbass_aac.so). They are automatically copied to the output folder when building via build.sh.

---

## Technologies Used

### BASS Audio Library

OnAirchive uses [BASS Audio Library](https://www.un4seen.com/) for working with audio streams:

- **BASS** - core library for streaming and decoding
- **BASSENC** - audio decoding
- **BASS_AAC** - plugin for AAC support

**License:** BASS is free for non-commercial use. Commercial use requires a license.

**Links:**
- Main site: https://www.un4seen.com/
- Documentation: https://www.un4seen.com/doc/

### FFmpeg

Used for audio encoding:

- **Windows:** Pre-compiled ffmpeg.exe included from [sudo-nautilus/FFmpeg-Builds-Win32](https://github.com/sudo-nautilus/FFmpeg-Builds-Win32/releases)
- **Linux:** Installed via package manager (`apt-get install ffmpeg`)

**License:** FFmpeg is distributed under GPLv2/LGPLv2.1 license

**Links:**
- Official site: https://ffmpeg.org/
- GitHub: https://github.com/FFmpeg/FFmpeg

---

## Project Structure

```
onairchive/
├── Core/                          # Platform-independent business logic
├── Recorder/                      # Recording functionality
├── RecorderFileStorage/           # File storage management
├── Logger/                        # Logging implementations
├── ThirdParty/                    # External library headers
├── Platforms/                     # Platform-specific entry points
│   ├── Windows/
│   │   ├── GUI/                   # Windows GUI application
│   │   │   ├── OnAirchiveVCL.dpr
│   │   │   ├── OnAirchiveVCL.dproj
│   │   │   └── uMainForm.pas
│   │   └── SVC/                   # Windows Service
│   │       ├── OnAirchiveSVC.dpr
│   │       ├── OnAirchiveSVC.dproj
│   │       └── OnAirchiveSVCServiceUnit.pas
│   └── Linux/                     # Linux Console application
│       ├── OnAirchiveCLI.lpr
│       ├── OnAirchiveCLI.lpi
│       ├── build.sh               # Build script
│       └── run-script.sh          # Run script
├── Libs/                          # Binary libraries
│   ├── Windows/
│   │   └── x86/                   # 32-bit Windows
│   │       ├── bass.dll
│   │       ├── bass_aac.dll
│   │       ├── bassenc.dll
│   │       └── ffmpeg.exe
│   └── Linux/
│       └── x86_64/                # 64-bit Linux
│           ├── libbass.so
│           ├── libbass_aac.so
│           └── libbassenc.so
├── Config/                        # Example configuration files
│   └── config-example.ini
├── Output/                        # Build results (in .gitignore)
│   ├── Windows/
│   └── Linux/
├── Temp/                          # Temporary build files (in .gitignore)
├── OnAirchive.groupproj           # Delphi group project
└── README.md                      # This file
```

## Contributing
If you found a bug or have suggestions for improvements, feel free to:

- Create an issue describing the problem or suggestion
- Submit pull requests with fixes or new features

## License

MIT
