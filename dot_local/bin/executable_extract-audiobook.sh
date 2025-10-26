#!/usr/bin/env bash
set -euo pipefail

FFMPEG="ffmpeg -hide_banner -loglevel error"
FFPROBE="ffprobe -v error"

function info() {
	echo "[INFO] $*"
}

function error() {
	echo "[ERROR] $*" >&2
	exit 1
}

function get_chapter_count() {
	local file="$1"
	$FFPROBE -show_chapters -of flat "$file" | grep -c 'chapters.chapter\.[0-9]*\.id' || echo "0"
}

function get_chapter_info() {
	local file="$1"
	local chapter_num="$2"
	local field="$3"

	$FFPROBE -show_chapters -of flat "$file" | grep "chapters.chapter.$chapter_num.$field" | cut -d '=' -f 2 | tr -d '"'
}

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
	echo "Usage: $0 <path_to_m4b_file>"
	exit 1
fi

M4B_FILE="$1"

# Check if the file exists and is a regular file
if [ ! -f "$M4B_FILE" ]; then
	error "File '$M4B_FILE' not found"
fi

# Check if the file is an m4b file
if [[ ! "$M4B_FILE" =~ \.m4b$ ]]; then
	error "File '$M4B_FILE' is not an m4b file"
fi

# Get the book name from the file (without extension)
BOOK_NAME=$(basename "$M4B_FILE" .m4b)
OUTPUT_DIR="$BOOK_NAME"

# Create output directory
if [ -d "$OUTPUT_DIR" ]; then
	info "Directory '$OUTPUT_DIR' already exists"
else
	info "Creating directory '$OUTPUT_DIR'..."
	mkdir -p "$OUTPUT_DIR"
fi

# Get chapter count
CHAPTER_COUNT=$(get_chapter_count "$M4B_FILE")

if [ "$CHAPTER_COUNT" -eq 0 ]; then
	error "No chapters found in '$M4B_FILE'"
fi

info "Found $CHAPTER_COUNT chapters in '$M4B_FILE'"

# Extract each chapter
for ((i = 0; i < CHAPTER_COUNT; i++)); do
	# Format chapter number with leading zeros (e.g., 01, 02, ...)
	chapter_num=$(printf "%02d" $((i + 1)))
	output_file="$OUTPUT_DIR/chapter-$chapter_num.mp3"

	# Skip if file already exists
	if [ -f "$output_file" ]; then
		info "Skipping chapter $chapter_num, file already exists..."
		continue
	fi

	# Get chapter start and end times
	start_time=$(get_chapter_info "$M4B_FILE" "$i" "start_time")
	end_time=$(get_chapter_info "$M4B_FILE" "$i" "end_time")

	# Get chapter title (optional)
	chapter_title=$(get_chapter_info "$M4B_FILE" "$i" "tags.title")

	if [ -n "$chapter_title" ]; then
		info "Extracting chapter $chapter_num: $chapter_title..."
	else
		info "Extracting chapter $chapter_num..."
	fi

	# Extract chapter using ffmpeg
	$FFMPEG -i "$M4B_FILE" \
		-ss "$start_time" \
		-to "$end_time" \
		-vn \
		-c:a libmp3lame \
		-q:a 2 \
		"$output_file"
done

info "Extraction complete! Files saved to '$OUTPUT_DIR'"
