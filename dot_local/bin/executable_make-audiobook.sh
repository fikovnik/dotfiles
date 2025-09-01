#!/usr/bin/env bash
set -euo pipefail

CATEGORY="audioteka"
FFMPEG="ffmpeg -hide_banner -loglevel error"

function info() {
	echo "[INFO] $*"
}

function get_mp3_tag() {
	local file="$1"
	local tag="$2"

	ffprobe -v error -show_entries format_tags="$tag" -of default=noprint_wrappers=1:nokey=1 "$file"
}

function largest_file() {
	local largest_file=""
	local largest_size=0

	for file in "$@"; do
		# Check if file exists and is a regular file
		if [[ -f "$file" ]]; then
			# Get the size of the current file
			current_size=$(gstat -c%s "$file")

			# Check if current size is larger than the largest so far
			if [[ $current_size -gt $largest_size ]]; then
				largest_size=$current_size
				largest_file="$file"
			fi
		fi
	done

	echo "$largest_file"
}

function create_metadata() {
	local dir="$1"
	local meta_file="$2"
	local list_file="$3"
	local playlist="$dir/playlist.pls"

	echo "" >"$list_file"
	echo ";FFMETADATA1" >"$meta_file"
	local metadata=0
	local start=0
	local title
	local title
	local length
	local album
	local artist

	while IFS= read -r line; do
		if [[ $line == File* ]]; then
			filename="$FOLDER_PATH/$(echo "$line" | cut -d '=' -f 2)"
			if [ $metadata -eq 0 ]; then
				metadata=1
				album=$(get_mp3_tag "$filename" album)
				artist=$(get_mp3_tag "$filename" artist)

				{
					echo "title=$album"
					echo "album=$album"
					echo "artist=$artist"
					echo "album_artist=$artist"
					echo "genre=$(get_mp3_tag "$filename" genre)"
					echo "date=$(get_mp3_tag "$filename" date)"
					echo "comment=$(get_mp3_tag "$filename" comment)"
					echo "category=$CATEGORY"
				} >>"$meta_file"
			fi
		elif [[ $line == Title* ]]; then
			title=$(echo "$line" | cut -d '=' -f 2)
		elif [[ $line == Length* ]]; then
			length=$(echo "$line" | cut -d '=' -f 2)
			echo "file '$filename'" >>"$list_file"

			{
				echo "[CHAPTER]"
				echo "TIMEBASE=1/1000"
				echo "START=$start"
				start=$((start + length))
				echo "END=$start"
				echo "title=$title"
			} >>"$meta_file"
		fi
	done <"$playlist"
}

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
	echo "Usage: $0 <path_to_folder>"
	exit 1
fi

FOLDER_PATH=""
if [[ -f "$1" && $(file -b --mime-type "$1") == application/zip ]]; then
	FOLDER_PATH=$(basename "$1" .zip)

	if [[ ! -d "$FOLDER_PATH" ]]; then
		info "Unzipping $1 into $FOLDER_PATH..."
		mkdir -p "$FOLDER_PATH"
		unzip -q "$1" -d "$FOLDER_PATH"
	else
		info "Skipping unzipping, $FOLDER_PATH already exists..."
	fi
elif [ -d "$1" ]; then
	FOLDER_PATH="$1"
else
	echo "Don't know what to do with $1"
	exit 1
fi

BOOK_NAME=$(basename "$FOLDER_PATH")

# Check if the provided path is a directory
if [ ! -d "$FOLDER_PATH" ]; then
	echo "Error: $FOLDER_PATH is not a directory"
	exit 1
fi

# Check if the cover image exists in the directory
COVER_FILE=$(largest_file "$FOLDER_PATH"/*.jpg)
if [ ! -f "$COVER_FILE" ]; then
	echo "Error: Cover image '$COVER_FILE' not found in the directory"
	exit 1
fi

# Output files
LIST_FILE="$BOOK_NAME.files.txt"
META_FILE="$BOOK_NAME.meta"
COMBINED_FILE="$BOOK_NAME.mp3"
CONVERTED_FILE="$BOOK_NAME.converted.m4b"
OUTPUT_FILE="$BOOK_NAME.m4b"

info "Cover file: $COVER_FILE"
info "List file: $LIST_FILE"
info "Meta file: $META_FILE"
info "Combined file: $COMBINED_FILE"
info "Converted file: $CONVERTED_FILE"
info "Output file: $OUTPUT_FILE"

info "Creating M4B metadata for $BOOK_NAME..."
create_metadata "$FOLDER_PATH" "$META_FILE" "$LIST_FILE"

if [ -f "$COMBINED_FILE" ]; then
	info "Skipping combining files, $COMBINED_FILE already exists..."
else
	info "Combining MP3 files into $COMBINED_FILE..."
	$FFMPEG -f concat -safe 0 -i "$LIST_FILE" -c copy "$COMBINED_FILE"
fi

if [ -f "$CONVERTED_FILE" ]; then
	info "Skipping M4B conversion, $CONVERTED_FILE already exists..."
else
	info "Converting $COMBINED_FILE into $CONVERTED_FILE..."
	$FFMPEG -i "$COMBINED_FILE" "$CONVERTED_FILE"
fi

if [ -f "$OUTPUT_FILE" ]; then
	info "Skipping metadata injection, $OUTPUT_FILE already exists..."
else
	info "Adding metadata..."
	$FFMPEG -i "$CONVERTED_FILE" \
		-i "$META_FILE" \
		-i "$COVER_FILE" \
		-map 0:a -map_metadata 1 -map 2:v -disposition:v:0 attached_pic -c copy -movflags +faststart \
		"$OUTPUT_FILE"
fi
