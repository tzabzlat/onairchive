#!/bin/bash
#
# Recording Station - Linux Build Script
# This script builds the console version using Free Pascal Compiler
#

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
PROJECT_NAME="OnAirchiveCLI"
PROJECT_FILE="${PROJECT_NAME}.lpr"
OUTPUT_DIR="../../Output/Linux"
TEMP_DIR="../../Temp/lib"
BINARY_NAME="onairchive"

# Detect architecture
ARCH=$(uname -m)
if [ "$ARCH" != "x86_64" ]; then
    echo -e "${RED}Unsupported architecture: $ARCH${NC}"
    echo "Only x86_64 is supported"
    exit 1
fi
TARGET_CPU="x86_64"

echo -e "${GREEN}Recording Station - Build Script${NC}"
echo "=================================="
echo "Architecture: $TARGET_CPU"
echo ""

# Check if fpc is installed
if ! command -v fpc &> /dev/null; then
    echo -e "${RED}Error: Free Pascal Compiler (fpc) not found${NC}"
    echo "Please install FPC:"
    echo "  Ubuntu/Debian: sudo apt-get install fpc"
    echo "  CentOS/RHEL:   sudo yum install fpc"
    echo "  Arch Linux:    sudo pacman -S fpc"
    exit 1
fi

FPC_VERSION=$(fpc -iV)
echo "FPC Version: $FPC_VERSION"
echo ""

# Check if ffmpeg is installed
if ! command -v ffmpeg &> /dev/null; then
    echo -e "${RED}Error: ffmpeg not found${NC}"
    echo "Please install ffmpeg:"
    echo "  Ubuntu/Debian: sudo apt-get install ffmpeg"
    echo "  CentOS/RHEL:   sudo yum install ffmpeg"
    echo "  Arch Linux:    sudo pacman -S ffmpeg"
    exit 1
fi

FFMPEG_VERSION=$(ffmpeg -version | head -n1)
echo "ffmpeg: $FFMPEG_VERSION"
echo ""

# Check if project file exists
if [ ! -f "$PROJECT_FILE" ]; then
    echo -e "${RED}Error: Project file not found: $PROJECT_FILE${NC}"
    exit 1
fi

# Create output directories
echo "Creating output directories..."
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR/${TARGET_CPU}-linux"

# Check for BASS libraries in Libs/Linux/x86_64
LIBS_DIR="../../Libs/Linux/${TARGET_CPU}"
if [ -f "$LIBS_DIR/libbass.so" ] && [ -f "$LIBS_DIR/libbassenc.so" ] && [ -f "$LIBS_DIR/libbass_aac.so" ]; then
    echo "Found BASS libraries in $LIBS_DIR"
else
    echo -e "${RED}Error: BASS libraries not found in $LIBS_DIR${NC}"
    echo "Required libraries: libbass.so, libbassenc.so, libbass_aac.so"
    echo "Download them from: https://www.un4seen.com/"
    echo "  - BASS: bass24-linux.zip"
    echo "  - BASSENC: bassenc24-linux.zip"
    echo "  - BASS AAC: bass_aac24-linux.zip"
    echo ""
    echo "Place all three files in Libs/Linux/${TARGET_CPU}/ directory"
    exit 1
fi

# Build configuration
echo -e "${GREEN}Building in RELEASE mode${NC}"
FPC_OPTIONS="-MObjFPC -Scghi -O3 -Xs -XX"

echo ""
echo "Build configuration:"
echo "  Output: $OUTPUT_DIR/$BINARY_NAME"
echo "  Temp: $TEMP_DIR/${TARGET_CPU}-linux"
echo ""

# Build
echo "Compiling..."
echo ""

# Build with BASS libraries
LIBS_PATH_ABS=$(cd "$LIBS_DIR" && pwd)
echo "Using BASS libraries from: $LIBS_PATH_ABS"
echo ""

fpc $FPC_OPTIONS \
    -Fi../.. -Fi../../Core -Fi../../Logger -Fi../../Recorder -Fi../../RecorderFileStorage -Fi../../ThirdParty \
    -Fu../.. -Fu../../Core -Fu../../Logger -Fu../../Recorder -Fu../../RecorderFileStorage -Fu../../ThirdParty \
    -Fl"$LIBS_PATH_ABS" \
    -FU"$TEMP_DIR/${TARGET_CPU}-linux" \
    -FE"$OUTPUT_DIR" \
    -o"$BINARY_NAME" \
    "$PROJECT_FILE"

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Build successful!${NC}"
    echo ""

    # Show file info
    if [ -f "$OUTPUT_DIR/$BINARY_NAME" ]; then
        FILE_SIZE=$(ls -lh "$OUTPUT_DIR/$BINARY_NAME" | awk '{print $5}')
        echo "Binary: $OUTPUT_DIR/$BINARY_NAME"
        echo "Size: $FILE_SIZE"
        echo ""

        # Make executable
        chmod +x "$OUTPUT_DIR/$BINARY_NAME"
        echo "Made executable"
        echo ""

        # Copy BASS libraries to output
        echo "Copying BASS libraries to output..."
        cp "$LIBS_DIR/libbass.so" "$LIBS_DIR/libbassenc.so" "$LIBS_DIR/libbass_aac.so" "$OUTPUT_DIR/"
        echo -e "${GREEN}✓${NC} Copied BASS libraries (including AAC plugin) to output directory"
        echo ""

        # Copy run script
        echo "Copying run script..."
        cp "run-script.sh" "$OUTPUT_DIR/run.sh"
        chmod +x "$OUTPUT_DIR/run.sh"
        echo -e "${GREEN}✓${NC} Copied run.sh script to output directory"
        echo ""

        # Copy example config
        echo "Copying example config..."
        cp "../../Config/config-example.ini" "$OUTPUT_DIR/config.ini"
        echo -e "${GREEN}✓${NC} Copied config.ini to output directory"
        echo ""

        echo ""
        echo "To run the application:"
        echo "  cd $OUTPUT_DIR"
        echo "  ./run.sh"
    fi
else
    echo ""
    echo -e "${RED}Build failed!${NC}"
    exit 1
fi