#!/bin/bash
# Copyright (c) 1998-2001 Robert Woodcock <rcw@debian.org>
# Copyright (c) 2003-2006 Jesus Climent <jesus.climent@hispalinux.es>
# This code is hereby licensed for public consumption under either the
# GNU GPL v2 or greater, or Larry Wall's Artistic license - your choice.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# Copyright for this work is to expire January 1, 2010, after which it
# shall be public domain.
#
# $Id: abcde 222M 2006-08-05 19:14:00Z (local) $

VERSION='2.3.99-$Revision: 222M $'

usage ()
{
echo "This is abcde v$VERSION."
echo "Usage: abcde [options] [tracks]"
echo "Options:"
echo "-1     Encode the whole CD in a single file"
echo "-a <action1[,action2]...>"
echo "       Actions to perform:"
echo "       cddb,read,normalize,encode,tag,move,replaygain,playlist,clean"
#echo "-A     Experimental actions (retag, transcode)"
echo "-b     Enable batch normalization"
#echo "-B     Disable batch replaygain (do file by file)"
echo "-c <file>"
echo "       Specify a configuration file (overrides system and user config files)"
echo "-C <discid#>"
echo "       Specify discid to resume from (only needed if you no longer have the cd)"
echo "-d <device>"
echo "       Specify CDROM device to grab (flac uses a single-track flac file)"
echo "-D     Debugging mode (equivalent to sh -x abcde)"
echo "-e     Erase encoded track information from status file"
echo "-f     Force operations that otherwise are considered harmful. Read \"man abcde\""
echo "-F     Skip check for audio CD and encode from WAV"
echo "-g     Use \"lame --nogap\" for MP3 encoding. Disables low disk and pipes flags"
echo "-h     This help information"
#echo "-i    Tag files while encoding, when possible (local only) -NWY-"
echo "-j <#> Number of encoder processes to run at once (localhost)"
echo "-k     Keep the wav tracks for later use"
echo "-l     Use low disk space algorithm"
echo "-L     Use local CDDB storage directory"
echo "-n     No lookup. Don't query CDDB, just create and use template"
echo "-N     Noninteractive. Never prompt for anything"
echo "-m     Modify playlist to include CRLF endings, to comply with some players"
echo "       WARNING: Deprecated. Use \"cue\" action"
echo "-M     Create a CUE file"
echo "-o <type1[,type2]...>"
echo "       Output file type(s) (vorbis,mp3,flac,spx,mpc,wav,m4a). Defaults to vorbis"
echo "-p     Pad track numbers with 0's (if less than 10 tracks)"
echo "-P     Use UNIX pipes to read+encode without wav files"
echo "-r <host1[,host2]...>"
echo "       Also encode on these remote hosts"
echo "-R     Use local CDDB in recursive mode"
echo "-s <field>"
echo "       Show dielfs from the CDDB info (year,genre)"
echo "-S <#> Set the CD speed"
echo "-t <#> Start the track numbering at a given number"
echo "-T <#> Same as -t but modifies tag numbering"
echo "-U     Do NOT use UNICODE (UTF8) tags and comments"
echo "-v     Show version number and exit"
echo "-V     Be a bit more verbose about what is happening behind the scenes"
echo "-x     Eject CD after all tracks are read"
echo "-w <comment>"
echo "       Add a comment to the CD tracks"
echo "-W <#> Contatenate CDs: -T #01 -w \"CD #\"" 
echo "-z     Use debug CDROMREADERSYNTAX option (needs cdparanoia)"
echo ""
echo "Tracks is a space-delimited list of tracks to grab."
echo "Ranges specified with hyphens are allowed (i.e., 1-5)."
echo ""
#echo "Double hyphens are used to concatenate tracks"
}

addstatus ()
{
	echo "$@" >> "$ABCDETEMPDIR/status"
}

# log [level] [message]
# 
# log outputs the right message in a common format
log ()
{
	BLURB="$1"
	shift
	case $BLURB in
		error)   echo "[ERROR] abcde: $@" >&2 ;;
		warning) echo "[WARNING] $@" >&2 ;;
		info)    echo "[INFO] $@" ;;
	esac
}

# Funtions to replace the need of seq, which is too distribution dependant.
f_seq_row ()
{
	i=$1
	while [ $i -ne `expr $2 + 1` ]
	do
		echo $i
		i=`expr $i + 1`
	done
}

f_seq_line ()
{
	i=$1
	if echo $i | grep "[[:digit:]]" > /dev/null 2>&1 ; then
		while [ $i -ne `expr $2 + 1` ]
		do
			printf $i" "
			i=`expr $i + 1`
		done
		echo
	else
		log error "syntax error while processing track numbers"
		exit 1
	fi
}

# Functions to replace the need of awk {print $1} and {print $NF}
get_first()
{
if [ X"$1" = "X" ]; then
	for first in `cat`; do
		break
	done
else
	first=$1
fi
echo $first
}

get_last()
{
if [ X"$1" = "X" ]; then
	for stdin in `cat`; do
		last=$stdin
	done
else
	for last in $@ ; do :; done
fi
echo $last
}

# checkstatus [blurb]
# Returns "0" if the blurb was found, returns 1 if it wasn't
# Puts the blurb content, if available, on stdout.
# Otherwise, returns "".
checkstatus ()
{
	# Take the last line in the status file if there's multiple matches
	PATTERN="^$1(=.*)?$"
	BLURB=$(egrep $PATTERN "$ABCDETEMPDIR/status" | tail -n 1)

	if [ -z "$BLURB" ]; then
		# No matches found
		return 1
	else
		# Matches found
		# See if there's a = in it
		if [ "$(echo $BLURB | grep -c =)" != "0" ]; then
			echo "$(echo $BLURB | cut -f2- -d=)"
		fi
		return 0
	fi
}

# checkwarnings [blurb]
# Returns "0" if the blurb was found (meaning there was an warning),
# returns 1 if it wasn't (yes this is a little backwards).
# Does not print the blurb on stdout.
# Otherwise, returns "".
checkwarnings ()
{
	if [ -e "$ABCDETEMPDIR/warnings" ]; then :; else
		return 1
	fi
	# Take the last line in the status file if there's multiple matches
	PATTERN="^$1(:.*)?$"
	BLURB="$(egrep $PATTERN "$ABCDETEMPDIR/warnings" | tail -n 1)"

	if [ -z "$BLURB" ]; then
		# negative, we did not have a negative...
		return 1
	else
		# affirmative, we had a negative...
		return 0
	fi
}

# checkerrors [blurb]
# Returns "0" if the blurb was found (meaning there was an error),
# returns 1 if it wasn't (yes this is a little backwards).
# Does not print the blurb on stdout.
# Otherwise, returns "".
checkerrors ()
{
	if [ -e "$ABCDETEMPDIR/errors" ]; then :; else
		return 1
	fi
	# Take the last line in the status file if there's multiple matches
	PATTERN="^$1(:.*)?$"
	BLURB="$(egrep $PATTERN "$ABCDETEMPDIR/errors" | tail -n 1)"

	if [ -z "$BLURB" ]; then
		# negative, we did not have a negative...
		return 1
	else
		# affirmative, we had a negative...
		return 0
	fi
}

# page [file]
# Finds the right pager in the system to display a file
page ()
{
	PAGEFILE="$1"
	# Use the debian sensible-pager wrapper to pick the pager
	# user has requested via their $PAGER environment variable
	if [ -x "/usr/bin/sensible-pager" ]; then
		/usr/bin/sensible-pager "$PAGEFILE"
	elif [ -x "$PAGER" ]; then
		# That failed, try to load the preferred editor, starting
		# with their PAGER variable
		$PAGER "$PAGEFILE"
		# If that fails, check for less
	elif [ -x /usr/bin/less ]; then
		/usr/bin/less -f "$PAGEFILE"
		# more should be on all UNIX systems
	elif [ -x /bin/more ]; then
		/bin/more "$PAGEFILE"
	else
		# No bananas, just cat the thing
		cat "$PAGEFILE" >&2
	fi
}

# run_command [blurb] [command...]
# Runs a command, silently if necessary, and updates the status file
run_command ()
{
	BLURB="$1"
	shift
	# See if this is supposed to be silent
	if [ "$(checkstatus encode-output)" = "loud" ]; then
		"$@" >&2
		RETURN=$?
	else
		# Special case for SMP, since
		# encoder output is never displayed, don't mute echos
		if [ -z "$BLURB" -a "$MAXPROCS" != "1" ]; then
			"$@" >&2
			RETURN=$?
		else
			"$@" >/dev/null 2>&1
			RETURN=$?
		fi
	fi
	case "$1" in
	normalize|normalize-audio)
		if [ "$RETURN" = "2" ]; then
			# File was already normalized.
			RETURN=0
		fi
		;;
	esac
	if [ "$RETURN" != "0" ]; then
		# Put an error in the errors file. For various reasons we
		# can't capture a copy of the program's output but we can
		# log what we attempted to execute and the error code
		# returned by the program.
		if [ "$BLURB" ]; then
			TWEAK="$BLURB: "
		fi
		echo "${TWEAK}returned code $RETURN: $@" >> "$ABCDETEMPDIR/errors"
		return $RETURN # Do not pass go, do not update the status file
	fi
	if [ "$BLURB" ]; then
		echo $BLURB >> "$ABCDETEMPDIR/status"
	fi
}

# relpath() and slash() are Copyright (c) 1999 Stuart Ballard and
# distributed under the terms of the GNU GPL v2 or later, at your option

# Function to determine if a word contains a slash.
slash ()
{
	case "$1" in
	*/*) return 0;;
	*) return 1;;
	esac
}

# Function to give the relative path from one file to another.
# Usage: relpath fromfile tofile
# eg relpath music/Artist/Album.m3u music/Artist/Album/Song.mp3
# (the result would be Album/Song.mp3)
# Output is relative path to $2 from $1 on stdout

# This code has the following restrictions:
# Multiple ////s are not collapsed into single /s, with strange effects.
# Absolute paths and ../s are handled wrong in FR (but they work in TO)
# If FR is a directory it must have a trailing /

relpath ()
{
	FR="$1"
	TO="$2"

	case "$TO" in
	/*) ;; # No processing is needed for absolute paths
	*)
		# Loop through common prefixes, ignoring them.
		while slash "$FR" && [ "$(echo "$FR" | cut -d/ -f1)" = "$(echo "$TO" | cut -d/ -f1)" ]
		do
			FR="$(echo "$FR" | cut -d/ -f2-)"
			TO="$(echo "$TO" | cut -d/ -f2-)"
		done
		# Loop through directory portions left in FR, adding appropriate ../s.
		while slash "$FR"
		do
			FR="$(echo "$FR" | cut -d/ -f2-)"
			TO="../$TO"
		done
		;;
	esac

	echo $TO
}

new_checkexec ()
{
	if [ ! "$@" = "" ]; then
		# Cut off any command-line option we added in
		X=$(echo $@ | cut -d' ' -f2)
		if [ "$(which $X)" = "" ]; then
			return 1
		elif [ ! -x $(which $X) ]; then
			return 2
		fi
	fi
	return 0
}

checkexec ()
{
	if [ ! "$@" = "" ]; then
		# Cut off any command-line option we added in
		X=$(echo $@ | cut -d' ' -f2)
		# Test for built-in abcde.function
		[ "$X" != "${X#abcde.}" ] && type $X >/dev/null 2>&1 && return
		if [ "$(which $X)" = "" ]; then
			log error "$X is not in your path." >&2
			log info  "Define the full path to the executable if it exists on your system." >&2
			exit 1
		elif [ ! -x "$(which $X)" ]; then
			log error "$X is not executable." >&2
			exit 1
		fi
	fi
}

# diffentries <filename> <max_value> <entry1>,<entry2>
# max_value: the range of entries goes from 1 to <max_value>
diffentries ()
{
	FILENAME=$1
	shift
	local CDDBDIFFCHOICES=$1
	shift
	local CDDBDIFFCHOICE="$@"
	if [ ! X"$DIFF" = "X" ]; then
		PARSECHOICE1=$(echo $CDDBDIFFCHOICE | cut -d"," -f1 | xargs printf %d 2>/dev/null)
		PARSECHOICE2=$(echo $CDDBDIFFCHOICE | cut -d"," -f2 | xargs printf %d 2>/dev/null)
		if [ $PARSECHOICE1 -lt 1 ] || [ $PARSECHOICE1 -gt $CDDBDIFFCHOICES ] || \
		   [ $PARSECHOICE2 -lt 1 ] || [ $PARSECHOICE2 -gt $CDDBDIFFCHOICES ] || \
		   [ $PARSECHOICE1 -eq $PARSECHOICE2 ]; then 
			echo "Invalid diff range. Please select two coma-separated numbers between 1 and $CDDBDIFFCHOICES" >&2
		else
			# We parse the 2 choices to diff, store them in temporary files and diff them.
			for PARSECHOICE in $(echo $CDDBDIFFCHOICE | tr , \ ); do
				do_cddbparse "$ABCDETEMPDIR/$FILENAME.$PARSECHOICE" > "$ABCDETEMPDIR/$FILENAME.parsechoice.$PARSECHOICE"
			done
			echo "Showing diff between choices $PARSECHOICE1 and $PARSECHOICE2..." > "$ABCDETEMPDIR/$FILENAME.diff"
			$DIFF $DIFFOPTS "$ABCDETEMPDIR/$FILENAME.parsechoice.$PARSECHOICE1" "$ABCDETEMPDIR/$FILENAME.parsechoice.$PARSECHOICE2" >> "$ABCDETEMPDIR/$FILENAME.diff"
			if [ $(cat "$ABCDETEMPDIR/$FILENAME.diff" | wc -l) -ge 24 ]; then
				page "$ABCDETEMPDIR/$FILENAME.diff"
			else
				cat "$ABCDETEMPDIR/$FILENAME.diff" >&2
			fi
		fi
	else
		echo "The diff program was not found in your path. Please choose a number between 0 and $CDDBDIFFCHOICES." >&2
	fi
}

# getcddbinfo
# Finds an specific field from cddbinfo
getcddbinfo()
{
	case $1 in
	TRACKNAME1)
		TRACKNAME="$(grep ^TTITLE$CDDBTRACKNUM= "$CDDBDATA" | head -n 1 | cut -f2- -d= | tr -d \[:cntrl:\] | sed 's/\ \+$//')"
		;;
	TRACKNAME)
		TRACKNAME="$(grep ^TTITLE$CDDBTRACKNUM= "$CDDBDATA" | cut -f2- -d= | tr -d \[:cntrl:\] | sed 's/\ \+$//')"
		;;
	esac
}

# gettracknum
# Get the track number we are going to use for different actions
gettracknum()
{
	if [ -n "$STARTTRACKNUMBER" ] ; then
		# Get the trackpadding from the current track
		CURRENTTRACKPADDING=$(echo -n $UTRACKNUM | wc -c)
		TRACKNUM=$( printf %0.${CURRENTTRACKPADDING}d $(expr ${UTRACKNUM} + ${STARTTRACKNUMBER} - $FIRSTTRACK ))
	else
		TRACKNUM=${UTRACKNUM}
	fi
}

# makeids
#
# Calculate cddb disc ids without requiring specialized helper programs.
# largely copied from cd-discid and musicbrainz examples.  some of the steps
# don't make sense, but they're necessary to match the ids generated by other
# programs.
#
## FIXME ## Right now, we get 2 frames more than with cue2discid ??
# data@petit:~$ sh /tmp/cue2discid /home/data/tmp/flac/01.Roisin_Murphy--Ruby_Blue.flac
# processing offsetimes 00:00:00 04:47:10 08:20:37 11:46:46 17:45:36 21:41:57 27:32:21 32:03:73 35:39:28 38:27:33 43:50:38 44:42:34
# 980b4b0c 12 150 21685 37687 53146 80061 97782 124071 144448 160603 173208 197438 201334 2895
# data@petit:~$ metaflac --export-cuesheet-to=- /home/data/tmp/flac/01.Roisin_Murphy--Ruby_Blue.flac| python /home/data/sources/abcde/trunk/examples/cue2discid
# 980b4b0c 12 150 21685 37687 53146 80061 97782 124071 144448 160603 173208 197438 201334 2893
#
# Variables: OFFSETS, TRACKS, LEADOUT, [LEADIN]
makeids ()
{
	if [ X"$LEADOUT" = "X" ]; then
		log warning "Error trying to calculate disc ids without lead-out information."
		exit 1
	fi

	# default to a two second lead-in
	IDMAGICNUM=150
	LEADIN=${LEADIN:=150}

	# number of cdframes per second
	CDFRAMES=75

	# reset cddb checksum for cddb disc-id calululation
	CDDBCKSUM=0

	COOKEDOFFSETS=""
	for OFFSET in $(echo $OFFSETS)
	do
		COOKEDOFFSETS="${COOKEDOFFSETS} $(($OFFSET + $LEADIN))"

		OFFSETTIME=$(( ($OFFSET + $LEADIN) / $CDFRAMES  ))
		while [ $OFFSETTIME -gt 0 ]; do
			CDDBCKSUM=$(($CDDBCKSUM + $OFFSETTIME % 10))
			OFFSETTIME=$(($OFFSETTIME / 10))
		done

	done

	COOKEDOFFSETS="${COOKEDOFFSETS:1}"  # eat the leading space

	PREGAP=$(($(echo $OFFSETS | cut -f1 -d' ')))
	TOTALTIME=$(( (($LEADOUT + $LEADIN + $PREGAP) / $CDFRAMES) - (($LEADIN + $PREGAP) / $CDFRAMES)))

	printf -v HEXSUM "%08lx" $(( ($CDDBCKSUM % 0xff) << 24 | $TOTALTIME << 8 | $TRACKS))
	TRACKINFO="${HEXSUM} $((TRACKS)) ${COOKEDOFFSETS} $((($LEADOUT + $LEADIN + $IDMAGICNUM) / $CDFRAMES))"
}

do_replaygain()
{
	if checkstatus replaygain; then :; else
		run_command "" echo "Adding replygain information..."
		for TMPOUTPUT in $( echo $OUTPUTTYPE | tr , \ )
		do
			case $TMPOUTPUT in
				vorbis|ogg)
					OUTPUT=$OGGOUTPUTCONTAINER
					;;
				flac)
					OUTPUT=$FLACOUTPUTCONTAINER
					;;
				*)
					OUTPUT=$TMPOUTPUT
					;;
			esac
			OUTPUTFILES=""
			REPLAYINDEX=0
			for UTRACKNUM in $TRACKQUEUE
			do
				CDDBTRACKNUM=$(expr $UTRACKNUM - 1)
				getcddbinfo TRACKNAME
				splitvarious
				TRACKFILE="$(mungefilename "$TRACKNAME")"
				ARTISTFILE="$(mungefilename "$TRACKARTIST")"
				ALBUMFILE="$(mungefilename "$DALBUM")"
		                YEAR=${CDYEAR:-$CDYEAR}
				gettracknum
				if [ "$ONETRACK" = "y" ]; then 
					if [ "$VARIOUSARTISTS" = "y" ]; then
						OUTPUTFILE="$(eval echo \""$VAONETRACKOUTPUTFORMAT"\")"
					else
						OUTPUTFILE="$(eval echo \""$ONETRACKOUTPUTFORMAT"\")"
					fi
				else	
					if [ "$VARIOUSARTISTS" = "y" ]; then
						OUTPUTFILE="$(eval echo \""$VAOUTPUTFORMAT"\")"
					else
						OUTPUTFILE="$(eval echo \""$OUTPUTFORMAT"\")"
					fi
				fi
				OUTPUTFILES[$REPLAYINDEX]="$OUTPUTDIR/$OUTPUTFILE.$OUTPUT"
				(( REPLAYINDEX = $REPLAYINDEX + 1 ))
			done
			case "$OUTPUT" in
				flac)
					run_command replaygain-flac nice $ENCNICE $METAFLAC --add-replay-gain "${OUTPUTFILES[@]}"
					;;
				vorbis|ogg)
					run_command replaygain-vorbis nice $ENCNICE $VORBISGAIN --album "${OUTPUTFILES[@]}"
					;;
				mp3)
					run_command replaygain-mp3 nice $ENCNICE $MP3GAIN -a "${OUTPUTFILES[@]}"
					;;
				mpc)
					run_command replaygain-mpc nice $ENCNICE $MPPGAIN --auto "${OUTPUTFILES[@]}"
					;;
				*);;
			esac
		done
		if checkerrors "replaygain-.{3,6}"; then :; else
			run_command replaygain true
		fi
	fi
}

# This code splits the a Various Artist track name from one of the following
# forms:
#
#  forward:        Artist / Track
#  forward-dash:   Artist - Track
#  reverse:        Track / Artist
#  reverse-dash:   Track - Artist
#  colon:          Artist: Track
#  trailing-paren: Artist (Track)
#
# variables used:
# VARIOUSARTISTS, VARIOUSARTISTSTYLE, TRACKNAME, TRACKARTIST
splitvarious ()
{
	if [ "$VARIOUSARTISTS" = "y" ] && [ ! "$ONETRACK" = "y" ]; then
		case "$VARIOUSARTISTSTYLE" in
		forward)
			DTITLEARTIST="$(echo "$TRACKNAME" | sed 's- / -~-g')"
			TRACKARTIST="$(echo "$DTITLEARTIST" | cut -f1 -d~)"
			TRACKNAME="$(echo "$DTITLEARTIST" | cut -f2 -d~)"
			;;
		forward-dash)
			DTITLEARTIST="$(echo "$TRACKNAME" | sed 's, - ,~,g')"
			TRACKARTIST="$(echo "$DTITLEARTIST" | cut -f1 -d~)"
			TRACKNAME="$(echo "$DTITLEARTIST" | cut -f2 -d~)"
			;;
		reverse)
			DTITLEARTIST="$(echo "$TRACKNAME" | sed 's- / -~-g')"
			TRACKARTIST="$(echo "$DTITLEARTIST" | cut -f2 -d~)"
			TRACKNAME="$(echo "$DTITLEARTIST" | cut -f1 -d~)"
			;;
		reverse-dash)
			DTITLEARTIST="$(echo "$TRACKNAME" | sed 's, - ,~,g')"
			TRACKARTIST="$(echo "$DTITLEARTIST" | cut -f2 -d~)"
			TRACKNAME="$(echo "$DTITLEARTIST" | cut -f1 -d~)"
			;;
		colon)
			DTITLEARTIST="$(echo "$TRACKNAME" | sed 's-: -~-g')"
			TRACKARTIST="$(echo "$DTITLEARTIST" | cut -f1 -d~)"
			TRACKNAME="$(echo "$DTITLEARTIST" | cut -f2 -d~)"
			;;
		trailing-paren)
			DTITLEARTIST="$(echo "$TRACKNAME" | sed 's,^\(.*\) (\(.*\)),\1~\2,')"
			TRACKARTIST="$(echo "$DTITLEARTIST" | cut -f2 -d~)"
			TRACKNAME="$(echo "$DTITLEARTIST" | cut -f1 -d~)"
			;;
		esac
	elif [ "$VARIOUSARTISTS" = "y" ] && [ "$ONETRACK" = "y" ]; then
		TRACKARTIST="Various"
	else
		TRACKARTIST="$DARTIST"
	fi
}

do_getgenreid () {
local genre=$(echo "${@}" | tr '[A-Z]' '[a-z]')
local id=""
	case ${genre} in
		"blues")                 id=0 ;;
		"classic rock")          id=1 ;;
		"country")               id=2 ;;
		"dance")                 id=3 ;;
		"disco")                 id=4 ;;
		"funk")                  id=5 ;;
		"grunge")                id=6 ;;
		"hip-hop")               id=7 ;;
		"jazz")                  id=8 ;;
		"metal")                 id=9 ;;
		"new age")               id=10 ;;
		"oldies")                id=11 ;;
		"other")                 id=12 ;;
		"pop")                   id=13 ;;
		"r&b")                   id=14 ;;
		"rap")                   id=15 ;;
		"reggae")                id=16 ;;
		"rock")                  id=17 ;;
		"techno")                id=18 ;;
		"industrial")            id=19 ;;
		"alternative")           id=20 ;;
		"ska")                   id=21 ;;
		"death metal")           id=22 ;;
		"pranks")                id=23 ;;
		"soundtrack")            id=24 ;;
		"euro-techno")           id=25 ;;
		"ambient")               id=26 ;;
		"trip-hop")              id=27 ;;
		"vocal")                 id=28 ;;
		"jazz+funk")             id=29 ;;
		"fusion")                id=30 ;;
		"trance")                id=31 ;;
		"classical")             id=32 ;;
		"instrumental")          id=33 ;;
		"acid")                  id=34 ;;
		"house")                 id=35 ;;
		"game")                  id=36 ;;
		"sound clip")            id=37 ;;
		"gospel")                id=38 ;;
		"noise")                 id=39 ;;
		"alt. rock")             id=40 ;;
		"bass")                  id=41 ;;
		"soul")                  id=42 ;;
		"punk")                  id=43 ;;
		"space")                 id=44 ;;
		"meditative")            id=45 ;;
		"instrum. pop")          id=46 ;;
		"instrum. rock")         id=47 ;;
		"ethnic")                id=48 ;;
		"gothic")                id=49 ;;
		"darkwave")              id=50 ;;
		"techno-indust.")        id=51 ;;
		"electronic")            id=52 ;;
		"pop-folk")              id=53 ;;
		"eurodance")             id=54 ;;
		"dream")                 id=55 ;;
		"southern rock")         id=56 ;;
		"comedy")                id=57 ;;
		"cult")                  id=58 ;;
		"gangsta")               id=59 ;;
		"top 40")                id=60 ;;
		"christian rap")         id=61 ;;
		"pop/funk"|"pop / funk") id=62 ;;
		"jungle")                id=63 ;;
		"native american")       id=64 ;;
		"cabaret")               id=65 ;;
		"new wave")              id=66 ;;
		"psychadelic")           id=67 ;;
		"rave")                  id=68 ;;
		"showtunes")             id=69 ;;
		"trailer")               id=70 ;;
		"lo-fi")                 id=71 ;;
		"tribal")                id=72 ;;
		"acid punk")             id=73 ;;
		"acid jazz")             id=74 ;;
		"polka")                 id=75 ;;
		"retro")                 id=76 ;;
		"musical")               id=77 ;;
		"rock & roll")           id=78 ;;
		"hard rock")             id=79 ;;
		"folk")                  id=80 ;;
		"folk/rock")             id=81 ;;
		"national folk")         id=82 ;;
		"swing")                 id=83 ;;
		"fusion")                id=84 ;;
		"bebob")                 id=85 ;;
		"latin")                 id=86 ;;
		"revival")               id=87 ;;
		"celtic")                id=88 ;;
		"bluegrass")             id=89 ;;
		"avantgarde")            id=90 ;;
		"gothic rock")           id=91 ;;
		"progress. rock")        id=92 ;;
		"psychadel. rock")       id=93 ;;
		"symphonic rock")        id=94 ;;
		"slow rock")             id=95 ;;
		"big band")              id=96 ;;
		"chorus")                id=97 ;;
		"easy listening")        id=98 ;;
		"acoustic")              id=99 ;;
		"humour")                id=100 ;;
		"speech")                id=101 ;;
		"chanson")               id=102 ;;
		"opera")                 id=103 ;;
		"chamber music")         id=104 ;;
		"sonata")                id=105 ;;
		"symphony")              id=106 ;;
		"booty bass")            id=107 ;;
		"primus")                id=108 ;;
		"porn groove")           id=109 ;;
		"satire")                id=110 ;;
		"slow jam")              id=111 ;;
		"club")                  id=112 ;;
		"tango")                 id=113 ;;
		"samba")                 id=114 ;;
		"folklore")              id=115 ;;
		"ballad")                id=116 ;;
		"power ballad")          id=117 ;;
		"rhythmic soul")         id=118 ;;
		"freestyle")             id=119 ;;
		"duet")                  id=120 ;;
		"punk rock")             id=121 ;;
		"drum solo")             id=122 ;;
		"a capella")             id=123 ;;
		"euro-house")            id=124 ;;
		"dance hall")            id=125 ;;
		"goa")                   id=126 ;;
		"drum & bass")           id=127 ;;
		"club-house")            id=128 ;;
		"hardcore")              id=129 ;;
		"terror")                id=130 ;;
		"indie")                 id=131 ;;
		"britpop")               id=132 ;;
		"negerpunk")             id=133 ;;
		"polsk punk")            id=134 ;;
		"beat")                  id=135 ;;
		"christian gangsta rap") id=136 ;;
		"heavy metal")           id=137 ;;
		"black metal")           id=138 ;;
		"crossover")             id=139 ;;
		"contemporary christian")id=140 ;;
		"christian rock")        id=141 ;;
		"merengue")              id=142 ;;
		"salsa")                 id=143 ;;
		"thrash metal")          id=144 ;;
		"anime")                 id=145 ;;
		"jpop")                  id=146 ;;
		"synthpop")              id=147 ;;
		"rock/pop"|"rock / pop") id=148 ;;
		*)                       return 1 ;;
	esac
echo ${id}
return 0
}

# do_tag [tracknumber]
# id3 tags a filename
# variables used:
# TRACKS, TRACKNAME, TRACKARTIST, TAGGER, TAGGEROPTS, VORBISCOMMENT, METAFLAC, 
# COMMENT, DALBUM, DARTIST, CDYEAR, CDGENRE (and temporarily) ID3TAGV
do_tag ()
{
	COMMENTOUTPUT="$(eval echo ${COMMENT})"
	CDDBDISCID=$(echo $TRACKINFO | cut -d' ' -f1)
	run_command '' echo "Tagging track $1 of $TRACKS: $TRACKNAME..."
	# If we want to start the tracks with a given number, we need to modify the
	# TRACKNUM value before evaluation
	if [ -n "$STARTTRACKNUMBERTAG" ] ; then
		gettracknum
	fi
	for OUTPUT in $(echo $OUTPUTTYPE | tr , \ )
	do
		case "$OUTPUT" in
		mp3)
			# id3v2 v0.1.9 claims to have solved the -c bug, so we merge both id3 and id3v2
			GENREID=$(do_getgenreid "${CDGENRE}")
	
			case "$ID3SYNTAX" in
				id3);;
				eyed3)
					# FIXME # track numbers in mp3 come with 1/10, so we cannot
					# happily substitute them with $TRACKNUM
					run_command tagtrack-$OUTPUT-$1 nice $ENCNICE $TAGGER $TAGGEROPTS \
						--comment=::"$COMMENTOUTPUT" -A "$DALBUM" \
						-a "$TRACKARTIST" -t "$TRACKNAME" -Y "$CDYEAR" \
						-G "$GENREID" -n "${TRACKNUM:-$1}" "${TRACKNUM:+-N $TRACKS}" \
						"${ENCODING:+--set-encoding=$ENCODING}" \
						"$ABCDETEMPDIR/track$1.$OUTPUT"
					;;
				# FIXME # Still not activated...
				id3ed)
					run_command tagtrack-$OUTPUT-$1 nice $ENCNICE $TAGGER $TAGGEROPTS -c "$COMMENTOUTPUT" \
						-a "$DALBUM" -n "$TRACKARTIST" -s "$TRACKNAME" -y "$CDYEAR" \
						-g "$GENREID" -k "${TRACKNUM:-$1}" \
						"$ABCDETEMPDIR/track$1.$OUTPUT"
					;;
				*)
					# FIXME # track numbers in mp3 come with 1/10, so we cannot
					# happily substitute them with $TRACKNUM
					run_command tagtrack-$OUTPUT-$1 nice $ENCNICE $TAGGER $TAGGEROPTS -c "$COMMENTOUTPUT" \
						-A "$DALBUM" -a "$TRACKARTIST" -t "$TRACKNAME" -y "$CDYEAR" \
						-g "$GENREID" -T "${TRACKNUM:-$1/$TRACKS}" \
						"$ABCDETEMPDIR/track$1.$OUTPUT"
					;;
			esac
			;;
		vorbis|ogg)
			case "$OGGENCODERSYNTAX" in
				vorbize|oggenc)
					# vorbiscomment can't do in-place modification, mv the file first
					if [ -f "$ABCDETEMPDIR/track$1.$OGGOUTPUTCONTAINER" -a ! -f "$ABCDETEMPDIR/track$1.uncommented.$OGGOUTPUTCONTAINER" ]; then
						mv "$ABCDETEMPDIR/track$1.$OGGOUTPUTCONTAINER" "$ABCDETEMPDIR/track$1.uncommented.$OGGOUTPUTCONTAINER"
					fi
					(
					# These are from http://www.xiph.org/ogg/vorbis/doc/v-comment.html
					echo ARTIST="$TRACKARTIST"
					echo ALBUM="$DALBUM"
					echo TITLE="$TRACKNAME"
					if [ -n "$CDYEAR" ]; then
						echo DATE="$CDYEAR"
					fi
					if [ -n "$CDGENRE" ]; then
						echo GENRE="$CDGENRE"
					fi	
					echo TRACKNUMBER=${TRACKNUM:-$1}
					echo CDDB=$CDDBDISCID
					if [ "$(eval echo ${COMMENT})" != "" ]; then
						case "$COMMENTOUTPUT" in
							*=*) echo "$COMMENTOUTPUT";;
							*)   echo COMMENT="$COMMENTOUTPUT";;
						esac	
					fi
					) | run_command tagtrack-$OUTPUT-$1 nice $ENCNICE $VORBISCOMMENT $VORBISCOMMENTOPTS -w \
						"$ABCDETEMPDIR/track$1.uncommented.$OGGOUTPUTCONTAINER" "$ABCDETEMPDIR/track$1.$OGGOUTPUTCONTAINER"
					# Doublecheck that the commented file was created successfully before wiping the original
					if [ -f "$ABCDETEMPDIR/track$1.$OGGOUTPUTCONTAINER" ]; then
						rm -f "$ABCDETEMPDIR/track$1.uncommented.$OGGOUTPUTCONTAINER"
					else
						mv "$ABCDETEMPDIR/track$1.uncommented.$OGGOUTPUTCONTAINER" "$ABCDETEMPDIR/track$1.$OGGOUTPUTCONTAINER"
					fi
					;;
			esac
			;;
		flac)
			(
			echo ARTIST="$TRACKARTIST"
			echo ALBUM="$DALBUM"
			echo TITLE="$TRACKNAME"
			if [ -n "$CDYEAR" ]; then
				echo DATE="$CDYEAR"
			fi
			if [ -n "$CDGENRE" ]; then
				echo GENRE="$CDGENRE"
			fi	
			echo TRACKNUMBER="${TRACKNUM:-$1}"
			echo CDDB="$CDDBDISCID"
			if [ "$(eval echo ${COMMENT})" != "" ]; then
				case "$COMMENTOUTPUT" in
					*=*) echo "$COMMENTOUTPUT";;
					*)   echo COMMENT="$COMMENTOUTPUT";;
				esac	
			fi
			) | run_command tagtrack-$OUTPUT-$1 nice $ENCNICE $METAFLAC $METAFLACOPTS ${IMPORTCUESHEET:+--import-cuesheet-from="$ABCDETEMPDIR/$CUEFILE"} --import-tags-from=- "$ABCDETEMPDIR/track$1.$FLACOUTPUTCONTAINER"
			;;
		spx)
			run_command tagtrack-$OUTPUT-$1 true
			;;
		mpc)
			run_command tagtrack-$OUTPUT-$1 true
			;;
		m4a)
			run_command tagtrack-$OUTPUT-$1 true
			;;
		wav)
			run_command tagtrack-$OUTPUT-$1 true
			;;
		esac
	done
	if checkerrors "tagtrack-(.{3,6})-$1"; then :; else
		run_command tagtrack-$1 true
	fi

}

# do_nogap_encode
# variables used:
# OUTPUTTYPE, {FOO}ENCODERSYNTAX, ENCNICE, ENCODER, ENCODEROPTS
do_nogap_encode ()
{
	# The commands here don't go through run_command because they're never supposed to be silenced
	echo "Encoding gapless MP3 tracks: $TRACKQUEUE"
	for OUTPUT in $(echo $OUTPUTTYPE | tr , \ )
	do
		case "$OUTPUT" in
		mp3)
			case "$MP3ENCODERSYNTAX" in
			lame|toolame)
				(
				cd "$ABCDETEMPDIR"
				TRACKFILES=
				for UTRACKNUM in $TRACKQUEUE
				do
					TRACKFILES="$TRACKFILES track$UTRACKNUM.wav"
				done
				nice $ENCNICE $MP3ENCODER $MP3ENCODEROPTS --nogap $TRACKFILES
				RETURN=$?
				if [ "$RETURN" != "0" ]; then
					echo "nogap-encode: $ENCODER returned code $RETURN" >> errors
				else
					for UTRACKNUM in $TRACKQUEUE
					do
						run_command encodetrack-$OUTPUT-$UTRACKNUM true
						#run_command encodetrack-$UTRACKNUM true
					done
				fi
				)
				;;
			esac
			;;
		esac
	done		
	if checkerrors "nogap-encode"; then :; else
		if [ ! "$KEEPWAVS" = "y" ] ; then
			if [ ! "$KEEPWAVS" = "move" ] ; then
				rm -f "$IN"
			fi
		fi
	fi
	# Other encoders fall through to normal encoding as the tracks
	# have not been entered in the status file.
}

# do_encode [tracknumber] [hostname]
# If no hostname is specified, encode locally
# variables used:
# TRACKS, TRACKNAME, TRACKARTIST, DISTMP3, DISTMP3OPTS, {FOO}ENCODERSYNTAX, OUTPUTTYPE, ENCODEROPTS, DALBUM, DARTIST, ENCNICE, CDYEAR, CDGENRE, COMMENT
do_encode ()
{
	if [ "$USEPIPES" = "y" ]; then
		case "$OUTPUT" in
			mp3)
				TEMPARG="PIPE_$MP3ENCODERSYNTAX"
				;;
			vorbis|ogg)
				TEMPARG="PIPE_$OGGENCODERSYNTAX"
				;;
			flac)
				TEMPARG="PIPE_$FLACENCODERSYNTAX"
				;;
			spx)
				TEMPARG="PIPE_$SPEEXENCODER"
				;;
			mpc)
				TEMPARG="PIPE_$MPPENCODER"
				;;
			m4a)
				TEMPARG="PIPE_$MPPENCODER"
				;;
		esac
		IN="$( eval echo "\$$TEMPARG" )"
	else
		IN="$ABCDETEMPDIR/track$1.wav"
		case "$OUTPUT" in
			mp3)
				case "$MP3ENCODERSYNTAX" in
					# FIXME # check if mp3enc needs -if for pipes
					# FIXME # I have not been able to find a working mp3enc binary
					mp3enc)
						FILEARG="-if $IN"
						;;
					*)
						FILEARG="$IN"
						;;
				esac
				;;
			*)
				FILEARG="$IN"
				;;
		esac
	fi
	# We need IN to proceed, if we are not using pipes.
	if [ -s "$IN" -o X"$USEPIPES" = "Xy" ] ; then
		for TMPOUTPUT in $(echo $OUTPUTTYPE | tr , \ )
		do
			case "$TMPOUTPUT" in
				vorbis|ogg)
					OUTPUT=$OGGOUTPUTCONTAINER
					;;
				flac)
					OUTPUT=$FLACOUTPUTCONTAINER
					;;
				*)
					OUTPUT=$TMPOUTPUT
					;;
			esac
			OUT="$ABCDETEMPDIR/track$1.$OUTPUT"
			if [ "$NOGAP" = "y" ] && checkstatus encodetrack-$OUTPUT-$1 ; then 
				continue
			fi
			if [ X"$USEPIPES" = "Xy" ]; then
				RUN_COMMAND=""
				# We need a way to store the creation of the files when using PIPES
				RUN_COMMAND_PIPES="run_command encodetrack-$OUTPUT-$1 true"
				# When pipping it does not make sense to have a higher nice for
				# reading than for encoding, since it will be hold by the
				# encoding process. Setting an effective nice, to calm down a
				# bit the reading process.
				EFFECTIVE_NICE=$READNICE
			else
				run_command '' echo "Encoding track $1 of $TRACKS: $TRACKNAME..."
				RUN_COMMAND="run_command encodetrack-$OUTPUT-$1"
				EFFECTIVE_NICE=$ENCNICE
			fi
			case "$OUTPUT" in
			mp3)
				case "$2" in
				%local*%)
					case "$MP3ENCODERSYNTAX" in
					lame|toolame|gogo) $RUN_COMMAND nice $EFFECTIVE_NICE $MP3ENCODER $MP3ENCODEROPTS "$IN" "$OUT" ;;
					bladeenc) $RUN_COMMAND nice $EFFECTIVE_NICE $MP3ENCODER $MP3ENCODEROPTS -quit "$IN" ;;
					l3enc|xingmp3enc) $RUN_COMMAND nice $EFFECTIVE_NICE $MP3ENCODER "$IN" "$OUT" $MP3ENCODEROPTS ;;
					# FIXME # Relates to the previous FIXME since it might need the "-if" removed.
					mp3enc) $RUN_COMMAND nice $EFFECTIVE_NICE $MP3ENCODER -if "$IN" -of "$OUT" $MP3ENCODEROPTS ;;
					esac
					;;
				*)
					$RUN_COMMAND nice $DISTMP3NICE $DISTMP3 $DISTMP3OPTS "$2" "$IN" "$OUT" >/dev/null 2>&1
					;;
				esac
				;;
			vorbis|ogg)
				case "$2" in
				%local*%)
					case "$OGGENCODERSYNTAX" in
					vorbize) $RUN_COMMAND nice $EFFECTIVE_NICE $OGGENCODER $OGGENCODEROPTS -w "$OUT" "$IN" ;;
					oggenc) $RUN_COMMAND nice $EFFECTIVE_NICE $OGGENCODER $OGGENCODEROPTS -o "$OUT" "$IN" ;;
					esac
					;;
				*)
					$RUN_COMMAND nice $DISTMP3NICE $DISTMP3 $DISTMP3OPTS "$2" "$IN" "$OUT" >/dev/null 2>&1
					;;
				esac
				;;
			flac)
				case "$2" in
				%local*%)
					case "$FLACENCODERSYNTAX" in
					flac) $RUN_COMMAND nice $EFFECTIVE_NICE $FLACENCODER -f $FLACENCODEROPTS -o "$OUT" "$IN" ;; 
				        esac
						;;
					*)
						vecho -n "DISTMP3:"
						vecho "$DISTMP3 $DISTMP3OPTS $2 $IN $OUT >/dev/null 2>&1"
						$RUN_COMMAND nice $DISTMP3NICE $DISTMP3 $DISTMP3OPTS "$2" "$IN" "$OUT" > /dev/null 2>&1
					;;
				esac
				;;
			spx)
				if [ "$(eval echo ${COMMENT})" != "" ]; then
					case "$COMMENT" in
						*=*) ;;
						*)   COMMENT="COMMENT=$COMMENT" ;;
					esac	
					COMMENT="--comment \"$COMMENT\""
				fi
				# Quick hack to avoid tagging Ogg/Speex, since there is no other way to tag than inline tagging
				if [ ! "$DOTAG" = "y" ]; then
					$RUN_COMMAND nice $EFFECTIVE_NICE $SPEEXENCODER $SPEEXENCODEROPTS --author "$TRACKARTIST" --title "$TRACKNAME" "$COMMENT" "$IN" "$OUT"
				else
					$RUN_COMMAND nice $EFFECTIVE_NICE $SPEEXENCODER $SPEEXENCODEROPTS "$IN" "$OUT"
				fi
				;;
			mpc)	
				# MPP/MP+(Musepack) format (.mpc) is done locally, with inline
				# tagging.
				# I tried compiling the mppenc from corecodecs.org and got some
				# errors, so I have not tried it myself.
				## FIXME ## Needs some cleanup to determine if an empty tag sent
				## FIXME ## to the encoder ends up empty.
				$RUN_COMMAND nice $EFFECTIVE_NICE $MPPENCODER $MPPENCODEROPTS --artist "$TRACKARTIST" --album "$DALBUM" --title "$TRACKNAME" --track "$1" --genre "$CDGENRE" --year "$CDYEAR" --comment "$COMMENT" "$IN" "$OUT"
				;;
			m4a)
				# Quick hack to avoid tagging Ogg/Speex, since there is no other way to tag than inline tagging
				if [ ! "$DOTAG" = "y" ]; then
					$RUN_COMMAND nice $EFFECTIVE_NICE $AACENCODER $AACENCODEROPTS --artist "$TRACKARTIST" --album "$DALBUM" --title "$TRACKNAME" --track "$1" --genre "$CDGENRE" --year "$CDYEAR" --comment "$COMMENT" -o "$OUT" "$IN"
					
				else
					$RUN_COMMAND nice $ENCNICE $AACENCODER $AACENCODEROPTS -o "$OUT" "$IN"
				fi
				;;
			wav)
				# In case of wav output we need nothing. Just keep the wavs.
				;;
			esac
			$RUN_COMMAND_PIPES
		done
		# Only remove .wav if the encoding succeeded
		if checkerrors "encodetrack-(.{3,6})-$1"; then :; else
			run_command encodetrack-$1 true
			if [ ! "$KEEPWAVS" = "y" ] ; then
				if [ ! "$KEEPWAVS" = "move" ] ; then
					rm -f "$IN"
				fi
			fi
		fi
	else
		run_command "" echo "HEH! The file we were about to encode disappeared:"
		run_command "" echo ">> $IN"
		run_command encodetrack-$1 false
	fi
}

# do_preprocess [tracknumber]
# variables used:
# TRACKS, TRACKNAME, TRACKARTIST, DISTMP3, DISTMP3OPTS, {FOO}ENCODERSYNTAX, OUTPUTTYPE, ENCODEROPTS, DALBUM, DARTIST, ENCNICE, CDYEAR, CDGENRE, COMMENT
#do_preprocess ()
#{
#	IN="$ABCDETEMPDIR/track$1.wav"
#	# We need IN to proceed.
#	if [ -s "$IN" ] ; then
#		for OUTPUT in $(echo $OUTPUTTYPE | tr , \ )
#		do
#			#OUT="$ABCDETEMPDIR/track$1.$OUTPUT"
#			run_command '' echo "Pre-processing track $1 of $TRACKS..."
#			case "$POSTPROCESSFORMAT" in
#			all|wav*)
#				run_command preprocess-$OUTPUT-$1 nice $PRENICE $WAV_PRE $IF $OF ;;
#			mp3)
#				run_command preprocess-$OUTPUT-$1 nice $PRENICE $MP3_PRE $IF $OF ;;
#			ogg)
#				run_command preprocess-$OUTPUT-$1 nice $PRENICE $OGG_PRE $IF $OF ;;
#			flac)
#				run_command preprocess-$OUTPUT-$1 nice $PRENICE $FLAC_PRE $IF $OF ;;
#			spx)
#				run_command preprocess-$OUTPUT-$1 nice $PRENICE $SPX_PRE $IF $OF ;;
#			esac
#		done
#		# Only remove .wav if the encoding succeeded
#		if checkerrors "preprocess-(.{3,4})-$1"; then 
#			run_command preprocess-$1 false
#		else
#			run_command preprocess-$1 true
#		fi
#	else
#		if [ "$(checkstatus encode-output)" = "loud" ]; then
#			echo "HEH! The file we were about to pre-process disappeared:"
#			echo ">> $IN"
#		fi
#		run_command preprocess-$1 false
#	fi
#}


# do_postprocess [tracknumber]
# variables used:
# TRACKS, TRACKNAME, TRACKARTIST, DISTMP3, DISTMP3OPTS, {FOO}ENCODERSYNTAX, OUTPUTTYPE, ENCODEROPTS, DALBUM, DARTIST, ENCNICE, CDYEAR, CDGENRE, COMMENT
#do_postprocess ()
#{
#	for POSTPROCESSFORMAT in $(echo $POSTPROCESSFORMATS | tr , \ )
#	do
#		IN="$ABCDETEMPDIR/track$1.$POSTPROCESSFORMAT"
#		# We need IN to proceed.
#		if [ -s "$IN" ] ; then
#			#OUT="$ABCDETEMPDIR/track$1.$OUTPUT"
#			run_command '' echo "Post-processing track $1 of $TRACKS..."
#			case "$POSTPROCESSFORMAT" in
#				mp3)
#					run_command postprocess-$OUTPUT-$1 nice $POSTNICE $MP3_POST $IF $OF ;;
#				ogg)
#					run_command postprocess-$OUTPUT-$1 nice $POSTNICE $OGG_POST $IF $OF ;;
#				flac)
#					run_command postprocess-$OUTPUT-$1 nice $POSTNICE $FLAC_POST $IF $OF ;;
#				spx)
#					run_command postprocess-$OUTPUT-$1 nice $POSTNICE $SPX_POST $IF $OF ;;
#			esac
#			# Only remove .wav if the encoding succeeded
#			if checkerrors "postprocess-(.{3,4})-$1"; then 
#				run_command postprocess-$1 false
#			else
#				run_command postprocess-$1 true
#			fi
#		else
#			if [ "$(checkstatus encode-output)" = "loud" ]; then
#				echo "HEH! The file we were about to post-process disappeared:"
#				echo ">> $IN"
#			fi
#			run_command postprocess-$1 false
#		fi
#	done
#}

# do_single_gain
# variables used:
# FIXME #
do_single_gain ()
{
:
}

# do_batch_gain
# variables used:
# MP3GAIN, MP3GAINOPTS, VORBISGAIN, VORBISGAINOPTS, MPPGAIN, MPPGAINOPTS
# FIXME #
do_batch_gain ()
{
	# The commands here don't go through run_command because they're never supposed to be silenced
	echo "Batch analizing gain in tracks: $TRACKQUEUE"
	(
	cd "$ABCDETEMPDIR"
	BLURB=
	TRACKFILES=
	for UTRACKNUM in $TRACKQUEUE
	do
		MP3FILES="$TRACKFILES track$UTRACKNUM.mp3"
	done
	# FIXME # Hard-coded batch option!
	$NORMALIZER -b $NORMALIZEROPTS $TRACKFILES
	RETURN=$?
	if [ "$RETURN" != "0" ]; then
		echo "batch-normalize: $NORMALIZER returned code $RETURN" >> errors
	else
		for UTRACKNUM in $TRACKQUEUE
		do
			echo normalizetrack-$UTRACKNUM >> status
		done
	fi
	)
}

# do_batch_normalize
# variables used:
# NORMALIZER, NORMALIZEROPTS
do_batch_normalize ()
{
	# The commands here don't go through run_command because they're never supposed to be silenced
	echo "Batch normalizing tracks: $TRACKQUEUE"
	(
	cd "$ABCDETEMPDIR"
	BLURB=
	TRACKFILES=
	for UTRACKNUM in $TRACKQUEUE
	do
		TRACKFILES="$TRACKFILES track$UTRACKNUM.wav"
	done
	# XXX: Hard-coded batch option!
	$NORMALIZER -b $NORMALIZEROPTS $TRACKFILES
	RETURN=$?
	if [ "$RETURN" != "0" ]; then
		echo "batch-normalize: $NORMALIZER returned code $RETURN" >> errors
	else
		for UTRACKNUM in $TRACKQUEUE
		do
			echo normalizetrack-$UTRACKNUM >> status
		done
	fi
	)
}

# do_normalize [tracknumber]
# variables used:
# TRACKS, TRACKNAME, NORMALIZER, NORMALIZEROPTS
do_normalize ()
{
	IN="$ABCDETEMPDIR/track$1.wav"
	if [ -e "$IN" ] ; then
		run_command '' echo "Normalizing track $1 of $TRACKS: $TRACKNAME..."
		run_command normalizetrack-$1 $NORMALIZER $NORMALIZEROPTS "$IN"
	else
		if [ "$(checkstatus encode-output)" = "loud" ]; then
			echo "HEH! The file we were about to normalize disappeared:"
			echo ">> $IN"
		fi
		run_command normalizetrack-$1 false "File $IN was not found"
	fi
}

# do_move [tracknumber]
# Deduces the outfile from environment variables
# Creates directory if necessary
# variables used:
# TRACKNUM, TRACKNAME, TRACKARTIST, DALBUM, OUTPUTFORMAT, CDGENRE, CDYEAR
do_move ()
{
	for TMPOUTPUT in $(echo $OUTPUTTYPE | tr , \ )
	do
		# For now, set OUTPUT as TMPOUTPUT, and then change it once we have
		# defined the OUTPUTFILE:
		OUTPUT="$TMPOUTPUT"

		# Create ALBUMFILE, ARTISTFILE, TRACKFILE
		# Munge filenames as follows:
		# ' ' -> '_'
		# '/' -> '_'
		# ''' -> ''
		# '?' -> ''
		# Eat control characters
		ALBUMFILE="$(mungefilename "$DALBUM")"
		ARTISTFILE="$(mungefilename "$TRACKARTIST")"
		TRACKFILE="$(mungefilename "$TRACKNAME")"
		GENRE="$(mungegenre "$GENRE")"
		YEAR=${CDYEAR:-$CDYEAR}
		# If we want to start the tracks with a given number, we need to modify
		# the TRACKNUM value before evaluation
		gettracknum
		# Supported variables for OUTPUTFORMAT are GENRE, YEAR, ALBUMFILE,
		# ARTISTFILE, TRACKFILE, and TRACKNUM.
		if [ "$ONETRACK" = "y" ]; then 
			if [ "$VARIOUSARTISTS" = "y" ]; then
				OUTPUTFILE="$(eval echo \""$VAONETRACKOUTPUTFORMAT"\")"
			else
				OUTPUTFILE="$(eval echo \""$ONETRACKOUTPUTFORMAT"\")"
		    	fi
		else	
			if [ "$VARIOUSARTISTS" = "y" ]; then
				OUTPUTFILE="$(eval echo \""$VAOUTPUTFORMAT"\")"
			else
				OUTPUTFILE="$(eval echo \""$OUTPUTFORMAT"\")"
			fi
 		fi
		if checkerrors "tagtrack-$OUTPUT-$1"; then :; else
			# Once we know the specific output was successful, we can change
			# the OUTPUT to the value containing the container
			case $TMPOUTPUT in
				vorbis|ogg)
					OUTPUT=$OGGOUTPUTCONTAINER
					;;
				flac)
					OUTPUT=$FLACOUTPUTCONTAINER
					;;
				*)
					OUTPUT=$TMPOUTPUT
					;;
			esac
			# Check that the directory for OUTPUTFILE exists, if it doesn't, create it
			OUTPUTFILEDIR="$(dirname "$OUTPUTDIR/$OUTPUTFILE")"
			case $OUTPUT in
				wav)
					if [ "$DOCLEAN" != "y" ] && [ "$FORCE" != "y" ]; then
						# FIXME # introduce warnings?
						:
					else
						# mkdir -p shouldn't return an error if the directory already exists
						mkdir -p "$OUTPUTFILEDIR"
						run_command movetrack-$1 mv "$ABCDETEMPDIR/track$1.$OUTPUT" "$OUTPUTDIR/$OUTPUTFILE.$OUTPUT"
						if checkstatus movetrack-output-$OUTPUT; then :; else
							run_command movetrack-output-$OUTPUT true
						fi
					fi
					;;
				*)
					# mkdir -p shouldn't return an error if the directory already exists
					mkdir -p "$OUTPUTFILEDIR"
					run_command movetrack-$1 mv "$ABCDETEMPDIR/track$1.$OUTPUT" "$OUTPUTDIR/$OUTPUTFILE.$OUTPUT"
					if checkstatus movetrack-output-$OUTPUT; then :; else
						run_command movetrack-output-$OUTPUT true
					fi
					;;
			esac
			# Lets move the cue file
			if CUEFILE=$(checkstatus cuefile) >/dev/null ; then 
				if [ -r "$ABCDETEMPDIR/$CUEFILE" ]; then
					if checkstatus movecue-$OUTPUT; then :; else
						# Silence the Copying output since it overlaps with encoding processes...
						#run_command '' vecho "Copying cue file to its destination directory..."
						if checkstatus onetrack >/dev/null ; then
							case $OUTPUT in
								wav)
									if [ "$DOCLEAN" != "y" ] && [ "$FORCE" != "y" ]; then
										# We dont have the dir, since it was not created before.
										:
									else
										run_command movecue-$OUTPUT cp "$ABCDETEMPDIR/$CUEFILE" "$OUTPUTDIR/$OUTPUTFILE.cue"
									fi
									;;
								# NOTE: Creating a cue file with the 3-char-extension files is to comply with
								# http://brianvictor.tripod.com/mp3cue.htm#details
								[a-z0-9][a-z0-9][a-z0-9])
									run_command movecue-$OUTPUT cp "$ABCDETEMPDIR/$CUEFILE" "$OUTPUTDIR/$OUTPUTFILE.cue"
									;;
								*)
									run_command movecue-$OUTPUT cp "$ABCDETEMPDIR/$CUEFILE" "$OUTPUTDIR/$OUTPUTFILE.$OUTPUT.cue"
									;;
							esac
						else
							run_command movecue-$OUTPUT cp "$ABCDETEMPDIR/$CUEFILE" "$OUTPUTFILEDIR/$CUEFILE"
						fi
						echo movecue-$OUTPUT >> "$ABCDETEMPDIR/status"
					fi
				fi
			fi
		fi
	done
}

# do_playlist
# Create the playlist if wanted
# Variables used:
# PLAYLISTFORMAT, PLAYLISTDATAPREFIX, VAPLAYLISTFORMAT, VAPLAYLISTDATAPREFIX,
# VARIOUSARTISTS, OUTPUTDIR
do_playlist ()
{
	for TMPOUTPUT in $(echo $OUTPUTTYPE | tr , \ )
	do
		case $TMPOUTPUT in
			vorbis|ogg)
				OUTPUT=$OGGOUTPUTCONTAINER
				;;
			flac)
				OUTPUT=$FLACOUTPUTCONTAINER
				;;
			*)
				OUTPUT=$TMPOUTPUT
				;;
		esac
		# Create a playlist file for the playlist data to go into.
		# We used to wipe it out if it existed. Now we request permision if interactive.
		for LASTTRACK in $TRACKQUEUE; do :; done
		ALBUMFILE="$(mungefilename "$DALBUM")"
		ARTISTFILE="$(mungefilename "$DARTIST")"
		GENRE="$(mungegenre "$GENRE")"
		YEAR=${CDYEAR:-$CDYEAR}
		if [ "$VARIOUSARTISTS" = "y" ] ; then
			PLAYLISTFILE="$(eval echo "$VAPLAYLISTFORMAT")"
		else
			PLAYLISTFILE="$(eval echo "$PLAYLISTFORMAT")"
		fi
		FINALPLAYLISTDIR="$(dirname "$OUTPUTDIR/$PLAYLISTFILE")"
		mkdir -p "$FINALPLAYLISTDIR"
		if [ -s "$OUTPUTDIR/$PLAYLISTFILE" ]; then
			echo -n "Erase, Append to, or Keep the existing playlist file? [e/a/k] (e): " >&2
			if [ "$INTERACTIVE" = "y" ]; then
				while [ "$DONE" != "y" ]; do
					read ERASEPLAYLIST
					case $ERASEPLAYLIST in
						e|E|a|A|k|K) DONE=y ;;
						*) ;;
					esac
				done
			else
				echo e >&2
				ERASEPLAYLIST=e
			fi
			# Once we erase the playlist, we use append to create the new one.
			[ "$ERASEPLAYLIST" = "e" -o "$ERASEPLAYLIST" = "E" ] && rm -f "$OUTPUTDIR/$PLAYLISTFILE" && ERASEPLAYLIST=a
		else
			# The playlist does not exist, so we can safelly use append to create the new list
			ERASEPLAYLIST=a
		fi
		if [ "$ERASEPLAYLIST" = "a" -o "$ERASEPLAYLIST" = "A" ]; then
			touch "$OUTPUTDIR/$PLAYLISTFILE"
			for UTRACKNUM in $TRACKQUEUE
			do
				# Shares some code with do_move since the filenames have to match
				CDDBTRACKNUM=$(expr $UTRACKNUM - 1)
				getcddbinfo TRACKNAME
				splitvarious
				TRACKFILE="$(mungefilename "$TRACKNAME")"
				ARTISTFILE="$(mungefilename "$TRACKARTIST")"
				ALBUMFILE="$(mungefilename "$DALBUM")"
				# If we want to start the tracks with a given number, we need to modify the
				# TRACKNUM value before evaluation
				gettracknum
				if [ "$VARIOUSARTISTS" = "y" ]; then
					OUTPUTFILE="$(eval echo \""$VAOUTPUTFORMAT\"")"
				else
					OUTPUTFILE="$(eval echo \""$OUTPUTFORMAT\"")"
				fi
				if [ "$VARIOUSARTISTS" = "y" ]; then
					if [ "$VAPLAYLISTDATAPREFIX" ] ; then
						echo ${VAPLAYLISTDATAPREFIX}$OUTPUTFILE.$OUTPUT >> "$OUTPUTDIR/$PLAYLISTFILE"
					else
						relpath "$PLAYLISTFILE", "$OUTPUTFILE.$OUTPUT" >> "$OUTPUTDIR/$PLAYLISTFILE"
					fi
				else
					if [ "$PLAYLISTDATAPREFIX" ]; then
						echo ${PLAYLISTDATAPREFIX}$OUTPUTFILE.$OUTPUT >> "$OUTPUTDIR/$PLAYLISTFILE"
					else
						relpath "$PLAYLISTFILE", "$OUTPUTFILE.$OUTPUT" >> "$OUTPUTDIR/$PLAYLISTFILE"
					fi
				fi
			done
		fi
		## this will convert the playlist to have CRLF line-endings, if specified
		## (some hardware players insist on CRLF endings)
		if [ "$DOSPLAYLIST" = "y" ]; then
			awk '{substr("\r",""); printf "%s\r\n", $0}' "$OUTPUTDIR/$PLAYLISTFILE" > "$ABCDETEMPDIR/PLAYLISTFILE.tmp"
#			mv -f "$ABCDETEMPDIR/PLAYLISTFILE.tmp" "$OUTPUTDIR/$PLAYLISTFILE"
			cat "$ABCDETEMPDIR/PLAYLISTFILE.tmp" | sed 's/\//\\/' > "$OUTPUTDIR/$PLAYLISTFILE"
		fi
		echo "playlistcomplete" >> "$ABCDETEMPDIR/status"
	done
}

# abcde.cue2discid
# This function reads a cuefile on stdin and writes an extended
# cddb query on stdout.  Any PREGAP for track 1 is properly
# handled, although cue files embedded in FLAC files do not
# appear to properly store the PREGAP setting. :(
abcde.cue2discid () {

	cddb_sum () {
		val=$1
		ret=0
		while [ $val -gt 0 ] ; do
			ret=$(( $ret + ( $val % 10) ))
			val=$(( $val / 10 ))
		done
		echo $ret
	}

	msf2lba () {
		OIFS="$IFS"
		IFS=":"
		set -- $1
		IFS="$OIFS"
		local first second third
		first=$(expr ${1} + 0 )
		second=$(expr ${2} + 0 )
		third=$(expr ${3} + 0 )

		echo $(( ((($first * 60) + $second) * 75) + $third ))
	}

	OFFSET=150
	PREGAP=0
	LEADOUT=0
	LEADIN=88200
	i=0
	N=0
	
	while read line ; do
		set -- $line
		case "$1" in
		TRACK)	i=$(( i + 1 ))
			;;
		INDEX)	if [ "$2" -eq 1 ] ; then
				LBA=$(msf2lba $3)
				START=$(( $LBA + $PREGAP + $OFFSET ))
				eval TRACK$i=$START
				X=$(cddb_sum $(( $START / 75 )) )
				N=$(( $N + $X ))
			fi
			;;
		PREGAP)	PREGAP=$(msf2lba $2)
			;;
		REM)	case "$2" in 
			FLAC__lead-out)
				LEADOUT=$(( $4 / 588 ))
				;;
			FLAC__lead-in)
				LEADIN=$(( $3 / 588 ))
				;;
			esac
			;;
		esac
		
	done
	
	TRACKS=$i
	LEADOUT=$(( $LEADOUT + $LEADIN ))

	LENGTH=$(( $LEADOUT/75 - $TRACK1/75 ))
	DISCID=$(( ( $N % 255 ) * 2**24 | $LENGTH * 2**8 | $TRACKS ))
	printf "%08x %i" $DISCID $TRACKS
	
	j=1
	while [ $j -le $TRACKS ] ; do
		eval echo -n "\" \$TRACK$j\""
		j=$((j+1))
	done
	echo " $(( $LEADOUT / 75 ))"
}

# abcde.mkcue
# abcde.mkcue [--wholedisk]
# This creates a cuefile directly from the extended discid information
# The --wholedisk option controls whether we're ripping data from the
# start of track one or from the start of the disk (usually, but not
# always the same thing!)
#
# Track one leadin/pregap (if any) handeling:
# --wholedisk specified:
#   TRACK 01 AUDIO
#     INDEX 00 00:00:00
#     INDEX 01 <pregap value>
#   Remaining track index values unchanged from disc TOC
#
# --wholedisk not specified
#   TRACK 01 AUDIO
#     PREGAP <pregap value>
#     INDEX 01 00:00:00
#   Remaining track index values offset by <pregap value>
#
# Variables used:
# TRACKINFO
abcde.mkcue () {

	echomsf () {
		printf "$1%02i:%02i:%02i\n" $(($2/4500)) $((($2/75)%60)) $(($2%75))
	} 

	local MODE DISCID TRACKS
	local i OFFSET LBA 


    if [ "$1" = --wholedisc ] ; then
        MODE=INDEX
    else
        MODE=PREGAP
    fi

    set -- $TRACKINFO

    DISCID=$1
    TRACKS=$2
    shift 2

    echo REM DISCID $DISCID
    echo FILE \"dummy.wav\" WAVE

    if [ $1 -ne 150 ] ; then
        if [ $MODE = PREGAP ] ; then
            OFFSET=$1
        else
            OFFSET=150
        fi
    fi

    i=1
    while [ $i -le "$TRACKS" ] ; do
        LBA=$(( $1 - $OFFSET ))
        printf "  TRACK %02i AUDIO\n" $i
        if [ $i -eq 1 -a $1 -ne 150 ] ; then
            if [ $MODE = PREGAP ] ; then
                echomsf "    PREGAP " $(($OFFSET-150))
            else
                echo    "    INDEX 00 00:00:00"
            fi
		fi
        echomsf "    INDEX 01 " $LBA
		i=$(($i+1))
        shift
    done
}

# do_discid
# This essentially the start of things
do_discid ()
{
	# Query the CD to get the track info, unless the user specified -C
	# or we are using some actions which do not need the CDDB data at all
	#if [ ! X"$EXPACTIONS" = "X" ]; then
	#	:
	#elif [ -z "$DISCID" ]; then
	if [ -z "$DISCID" ]; then
		vecho -n "Getting CD track info... "
		# In OSX, unmount the disc before a query
		if [ "$OSFLAVOUR" = "OSX" ]; then
			disktool -u ${CDROM#/dev/}
		fi
		case "$CDROMREADERSYNTAX" in
			flac)
				if $METAFLAC $METAFLACOPTS --export-cuesheet-to=- "$CDROM" > /dev/null 2>&1 ; then
					case "$CUE2DISCID" in
						# FIXME # right now we have 2 cue2discid internal
						# implementations: builtin and abcde.cue2discid. Test
						# both of them and decide which one we want to use.
						builtin)
							#vecho "Using builtin cue2discid implementation..."
							CUESHEET="$(metaflac $METAFLACOPTS --export-cuesheet-to=- "$CDROM")"

							#TRACKS=$(echo $CUESHEET | egrep "TRACK \+[[:digit:]]\+ \+AUDIO" |wc -l)
							#TRACKS=0
							OFFSETTIMES=( $(echo "$CUESHEET" | sed -n -e's/\ *INDEX 01\ \+//p' ) )
							TRACKS=${#OFFSETTIMES[@]}
							unset OFFSETS
							#echo "processing offsetimes ${OFFSETTIMES[@]}"
							for OFFSETTIME in ${OFFSETTIMES[@]}; do
								OFFSETS="$OFFSETS $(( 10#${OFFSETTIME:0:2} * 4500 + 10#${OFFSETTIME:3:2} * 75 + 10#${OFFSETTIME:6:2} ))"
								#OFFSETS[${#OFFSETS[*]}]=$(( 10#${OFFSETTIME:0:2} * 4500 + 10#${OFFSETTIME:3:2} * 75 + 10#${OFFSETTIME:6:2} ))
							done

							LEADOUT=$(( $(echo "$CUESHEET" | grep lead-out | get_last) / 44100 * 75 ))
							LEADIN=$(( $(echo "$CUESHEET" | grep lead-in | get_last) / 44100 * 75 ))
							makeids
						;;
						*)
							#vecho "Using external python cue2discid implementation..."
							TRACKINFO=$($METAFLAC $METAFLACOPTS --export-cuesheet-to=- "$CDROM" | $CUE2DISCID)
						;;
					esac
				else
					log error "the input flac file does not contain a cuesheet."
					exit 1
				fi
				;;
#			cdparanoia|debug)
#				CDPARANOIAOUTPUT="$( $CDROMREADER -$CDPARANOIACDROMBUS "$CDROM" -Q --verbose 2>&1 )"
#				RET=$?
#				if [ ! "$RET" = "0" ];then
#					log warning "something went wrong while querying the CD... Maybe a DATA CD?"
#				fi
#
#				TRACKS="$(echo "$CDPARANOIAOUTPUT" | egrep '^[[:space:]]+[[:digit:]]' | tail -n 1 | get_first | tr -d "." | tr '\n' ' ')"
#				CDPARANOIAAUDIOTRACKS="$TRACKS"
#
#				LEADOUT="$(echo "$CDPARANOIAOUTPUT" | egrep -o '^TOTAL[[:space:]]+([[:digit:]]+)' | get_last)"
#				OFFSETS="$(echo "$CDPARANOIAOUTPUT" | sed -n -e's/^ .* \([0-9]\+\) \[.*/\1/p')" 
#				makeids
#				;;
			*)
				case "$CDDBMETHOD" in
					cddb) TRACKINFO=$($CDDISCID "$CDROM") ;;
					# FIXME # musicbrainz needs a cleanup
					musicbrainz) TRACKINFO=$($MUSICBRAINZ -c "$CDROM" ) ;;
				esac
				;;
		esac
		# Make sure there's a CD in there by checking cd-discid's return code
		if [ ! "$?" = "0" ]; then
			if [ "$CDROMREADERSYNTAX" = "flac" ] ; then
				log error "cuesheet information from the flac file could not be read."
				log error "Perhaps the flac file does not contain a cuesheet?."
				exit 1
			else
				log error "CD could not be read. Perhaps there's no CD in the drive?"
				exit 1
			fi
		fi
		# In OSX, remount the disc again
		if [ "$OSFLAVOUR" = "OSX" ]; then
			disktool -m ${CDROM#/dev/}
		fi
		WEHAVEACD=y
		DISCID=$(echo $TRACKINFO | cut -f1 -d' ')
	else
		TRACKINFO=$(cat "$WAVOUTPUTDIR/abcde.$DISCID/discid")
	fi

	# Get a full enumeration of tracks, sort it, and put it in the TRACKQUEUE.
	# This needs to be done now because a section of the resuming code will need
	# it later.

	# get the number of digits to pad TRACKNUM with - we'll use this later
	# a CD can only hold 99 tracks, but since we support a feature for starting
	# numbering the tracks from a given number, we might need to set it as a
	# variable for the user to define... or obtain it somehow.
	if [ "$PADTRACKS" = "y" ] ; then
		TRACKNUMPADDING=2
	fi

	ABCDETEMPDIR="$WAVOUTPUTDIR/abcde.$(echo $TRACKINFO | cut -f1 -d' ')"
	if [ -z "$TRACKQUEUE" ]; then
		if [ ! "$STRIPDATATRACKS" = "n" ]; then
			case "$CDROMREADERSYNTAX" in
				cdparanoia|debug)
					if [ "$WEHAVEACD" = "y" ]; then
						vecho "Querying the CD for audio tracks..."
						CDPARANOIAOUTPUT="$( $CDROMREADER -$CDPARANOIACDROMBUS "$CDROM" -Q --verbose 2>&1 )"
						RET=$?
						if [ ! "$RET" = "0" ];then
							log warning "something went wrong while querying the CD... Maybe a DATA CD?"
						fi
						TRACKS="$(echo "$CDPARANOIAOUTPUT" | egrep '^[[:space:]]+[[:digit:]]' | tail -n 1 | get_first | tr -d "." | tr '\n' ' ')"
						CDPARANOIAAUDIOTRACKS="$TRACKS"
					else
						# Previous versions of abcde would store the tracks on a file, instead of the status record.
						if [ -f "$ABCDETEMPDIR/cdparanoia-audio-tracks" ]; then
							echo cdparanoia-audio-tracks=$( cat "$ABCDETEMPDIR/cdparanoia-audio-tracks" ) >> "$ABCDETEMPDIR/status"
							rm -f "$ABCDETEMPDIR/cdparanoia-audio-tracks"
						fi
						if [ -f "$ABCDETEMPDIR/status" ] && TRACKS=$(checkstatus cdparanoia-audio-tracks); then :; else
							TRACKS=$(echo $TRACKINFO | cut -f2 -d' ')
						fi
					fi
					;;
				*)	TRACKS=$(echo $TRACKINFO | cut -f2 -d' ') ;;
			esac
		else
			TRACKS=$(echo $TRACKINFO | cut -f2 -d' ')
		fi
		if echo "$TRACKS" | grep "[[:digit:]]" > /dev/null 2>&1 ;then :;else
			log info "The disc does not contain any tracks. Giving up..."
			exit 0
		fi
		echo -n "Grabbing entire CD - tracks: "
		if [ ! "$PADTRACKS" = "y" ] ; then
			TRACKNUMPADDING=$(echo -n $TRACKS | wc -c | tr -d ' ')
		fi
		TRACKS=$(printf "%0.${TRACKNUMPADDING}d" $TRACKS)
		X=0
		while [ "$X" -ne "$TRACKS" ]
		do
			X=$(printf "%0.${TRACKNUMPADDING}d" $(expr $X + 1))
			TRACKQUEUE=$(echo $TRACKQUEUE $X)
		done
		echo $TRACKQUEUE
	else
		TRACKS=$(echo $TRACKINFO | cut -f2 -d' ')
		# User-supplied track queue.
		# Weed out non-numbers, whitespace, then sort and weed out duplicates
		TRACKQUEUE=$(echo $TRACKQUEUE | sed 's-[^0-9 ]--g' | tr ' ' '\n' | grep -v ^$ | sort -n | uniq | tr '\n' ' ' | sed 's- $--g')
		# Once cleaned, obtain the highest value in the trackqueue for number padding
		for LASTTRACK in $TRACKQUEUE; do :; done
		if [ ! "$PADTRACKS" = "y" ] ; then
			TRACKNUMPADDING=$(echo -n $LASTTRACK | wc -c | tr -d ' ')
		fi
		# Now we normalize the trackqueue
		for TRACK in $TRACKQUEUE ; do
			TRACKNUM=$(printf %0.${TRACKNUMPADDING}d $(expr ${TRACK} + 0 ))
			PADTRACKQUEUE=$(echo $PADTRACKQUEUE $TRACKNUM)
		done
		TRACKQUEUE=$PADTRACKQUEUE
		echo Grabbing tracks: "$TRACKQUEUE"
	fi

	QUEUEDTRACKS=$(echo $TRACKQUEUE | wc -w | tr -d ' ')

	# We have the discid, create a temp directory after it to store all the temp
	# info

	if [ -e "$ABCDETEMPDIR" ]; then
		echo -n "abcde: attempting to resume from $ABCDETEMPDIR"
		# It already exists, see if it's a directory
		if [ ! -d "$ABCDETEMPDIR" ]; then
			# This is a file/socket/fifo/device/etc, not a directory
			# Complain and exit
			echo >&2
			echo "abcde: file $ABCDETEMPDIR already exists and does not belong to abcde." >&2
			echo "Please investigate, remove it, and rerun abcde." >&2
			exit 1
		fi
		echo -n .
		# It's a directory, let's see if it's owned by us
		if [ ! -O "$ABCDETEMPDIR" ]; then
			# Nope, complain and exit
			echo >&2
			echo "abcde: directory $ABCDETEMPDIR already exists and is not owned by you." >&2
			echo "Please investigate, remove it, and rerun abcde." >&2
			exit 1
		fi
		echo .
		# See if it's populated
		if [ ! -f "$ABCDETEMPDIR/discid" ]; then
			# Wipe and start fresh
			echo "abcde: $ABCDETEMPDIR/discid not found. Abcde must remove and recreate" >&2
			echo -n "this directory to continue. Continue? [y/n] (n)" >&2
			if [ "$INTERACTIVE" = "y" ]; then
				read ANSWER
			else
				echo y >&2
				ANSWER=y
			fi
			if [ "$ANSWER" != "y" ]; then
				exit 1
			fi
			rm -rf "$ABCDETEMPDIR" || exit 1
			mkdir -p "$ABCDETEMPDIR"
			if [ "$?" -gt "0" ]; then
				# Directory already exists or could not be created
				echo "abcde: Temp directory $ABCDETEMPDIR could not be created." >&2
				exit 1
			fi
		else
			# Everything is fine. Check for ^encodetracklocation-
			# and encode-output entries in the status file and
			# remove them. These are not relevant across sessions.
			if [ -f "$ABCDETEMPDIR/status" ]; then
				mv "$ABCDETEMPDIR/status" "$ABCDETEMPDIR/status.old"
				grep -v ^encodetracklocation- < "$ABCDETEMPDIR/status.old" \
					| grep -v ^encode-output > "$ABCDETEMPDIR/status"
			fi
			# Remove old error messages
			if [ -f "$ABCDETEMPDIR/errors" ]; then
				rm -f "$ABCDETEMPDIR/errors"
			fi
		fi
	else
		# We are starting from scratch
		mkdir -p "$ABCDETEMPDIR"
		if [ "$?" -gt "0" ]; then
			# Directory already exists or could not be created
			echo "abcde: Temp directory $ABCDETEMPDIR could not be created." >&2
			exit 1
		fi
		cat /dev/null > "$ABCDETEMPDIR/status"
		# Store the abcde version in the status file.
		echo "abcde-version=$VERSION" >> "$ABCDETEMPDIR/status"
	fi
	if [ X"$DOCUE" = "Xy" -a X"$WEHAVEACD" = "Xy" ]; then
		if checkstatus cuefile > /dev/null 2>&1 ; then :; else
			CUEFILE=cue-$(echo "$TRACKINFO" | cut -f1 -d' ').txt
			vecho "Creating cue file..."
			case $CDROMREADERSYNTAX in
				flac)
					if $METAFLAC --export-cuesheet-to=- "$CDROM" > "$ABCDETEMPDIR/$CUEFILE"; then 
						echo cuefile=$CUEFILE >> "$ABCDETEMPDIR/status"
					else
						log warning "the input flac file does not contain a cuesheet."
					fi
					;;
				*)
					if $CUEREADER $CUEREADEROPTS > "$ABCDETEMPDIR/$CUEFILE"; then
						echo cuefile=$CUEFILE >> "$ABCDETEMPDIR/status"
					else
						log warning "reading the CUE sheet is still considered experimental"
						log warning "and there was a problem with the CD reading. abcde will continue,"
						log warning "but consider reporting the problem to the abcde author"
					fi
					;;
			esac
		fi
	fi
	# If we got the CDPARANOIA status and it is not recorded, save it now
	if [ -n "$CDPARANOIAAUDIOTRACKS" ]; then
		if checkstatus cdparanoia-audio-tracks > /dev/null 2>&1; then :; else
			echo cdparanoia-audio-tracks=$CDPARANOIAAUDIOTRACKS >> "$ABCDETEMPDIR/status"
		fi
	fi
	
	# Create the discid file
	echo "$TRACKINFO" > "$ABCDETEMPDIR/discid"
	if checkstatus cddbmethod > /dev/null 2>&1 ; then :; else
		echo "cddbmethod=$CDDBMETHOD" >> "$ABCDETEMPDIR/status"
	fi
}

# do_cleancue
# Create a proper CUE file based on the CUE file we created before.
do_cleancue()
{
	if CUEFILE_IN="$ABCDETEMPDIR"/$(checkstatus cuefile); then
		CUEFILE_OUT=$CUEFILE_IN.out
		### FIXME ### checkstatus cddb
		if [ -e "$CDDBDATA" ]; then
			vecho "Adding metadata to the cue file..."
			# FIXME It doesn't preserve spaces! Why?
			# FIXME parse $track into PERFORMER and TITLE - abcde already has code for this?
			n=1
			echo "PERFORMER \"$DARTIST\"" >> "$CUEFILE_OUT"
			echo "TITLE \"$DALBUM\"" >> "$CUEFILE_OUT"
			# Set IFS to <newline> to prevent read from swallowing spaces and tabs
			OIFS="$IFS"
			IFS='
'
			cat "$CUEFILE_IN" | while read line
			do
				if echo "$line" | grep "INDEX 01" > /dev/null 2>&1 ; then
# FIXME # Possible patch: remove the line above, uncomment the 2 lines below.
#				echo "$line" >> "$CUEFILE_OUT"
#				if echo "$line" | grep "^[[:space:]]*TRACK" > /dev/null 2>&1 ; then
					eval track="\$TRACK$n"
					n=$(expr $n + 1)
					echo "    TITLE \"$track\"" >> "$CUEFILE_OUT"
				fi
# FIXME # If the lines above are uncommented, remove the line below.
				echo "$line" >> "$CUEFILE_OUT"
			done
			IFS="$OIFS"
			mv "$CUEFILE_OUT" "$CUEFILE_IN"
			echo "cleancuefile" >> "$ABCDETEMPDIR/status"
		fi
	fi
}

# do_cddbparse
# Parses a CDDB file and outputs the title and the track names.
# Variables: CDDBFILE
do_cddbparse ()
{
	CDDBPARSEFILE="$1"
	# List out disc title/author and contents
	if [ "$ONETRACK" = "y" ]; then
		vecho "ONETRACK mode selected: displaying only the title of the CD..."
	fi
	echo "---- $(grep DTITLE "${CDDBPARSEFILE}" | cut '-d=' -f2- | tr -d \\r\\n ) ----"
	if [ X"$SHOWCDDBYEAR" = "Xy" ]; then
		PARSEDYEAR=$(grep DYEAR "${CDDBPARSEFILE}" | cut '-d=' -f2-)
		if [ ! X"$PARSEDYEAR" = "X" ]; then
			echo "Year: $PARSEDYEAR"
		fi
	fi
	if [ X"$SHOWCDDBGENRE" = "Xy" ]; then
		PARSEDGENRE=$(grep DGENRE "${CDDBPARSEFILE}" | cut '-d=' -f2-)
		if [ ! X"$PARSEDGENRE" = "X" ]; then
			echo "Genre: $PARSEDGENRE"
		fi
	fi
	if [ ! "$ONETRACK" = "y" ]; then
		for TRACK in $(f_seq_row 1 $TRACKS)
		do
			echo $TRACK: "$(grep ^TTITLE$(expr $TRACK - 1)= "${CDDBPARSEFILE}" | cut -f2- -d= | tr -d \\r\\n)"
		done
	fi
}

# do_localcddb
# Check for a local CDDB file, and report success
do_localcddb ()
{
	if checkstatus cddb-readcomplete && checkstatus cddb-choice >/dev/null; then :; else
	
		CDDBLOCALSTATUS="notfound"
		CDDBDISCID=$(echo $TRACKINFO | cut -d' ' -f1)
		USELOCALRESP="y"

		if [ "$CDDBLOCALRECURSIVE" = "y" ]; then
			CDDBLOCALRESULTS="$(find ${CDDBLOCALDIR} -name "${CDDBDISCID}" -type f 2> /dev/null)"
			if [ ! "${CDDBLOCALRESULTS}" = "" ]; then
				if   (( $(echo "${CDDBLOCALRESULTS}" | wc -l) == 1 )); then
					CDDBLOCALFILE="${CDDBLOCALRESULTS}"
					CDDBLOCALMATCH=single
				elif (( $(echo "${CDDBLOCALRESULTS}" | wc -l) > 1 )); then
					CDDBLOCALMATCH=multiple
				fi
			else
				CDDBLOCALMATCH=none
			fi
		elif [ "$CDDBLOCALMATCH" = "none" ] && [ -r "${CDDBLOCALDIR}/${CDDBDISCID}" ]; then
			CDDBLOCALFILE="${CDDBLOCALDIR}/${CDDBDISCID}"
			CDDBLOCALMATCH=single
		else
			CDDBLOCALMATCH=none
		fi
		
		# If the user has selected to check a local CDDB repo, we proceed with it
		case $CDDBLOCALMATCH in
			multiple)
				echo "Processing multiple matching CDDB entries..." > "$ABCDETEMPDIR/cddblocalchoices"
				X=0
				echo "$CDDBLOCALRESULTS" | while read RESULT ; do
					X=$(expr $X + 1)
					# List out disc title/author and contents
					CDDBLOCALREAD="$ABCDETEMPDIR/cddblocalread.$X"
					cat "$RESULT" > "${CDDBLOCALREAD}"
					{	
						echo -n "#$X: "
						do_cddbparse "${CDDBLOCALREAD}" 
						echo ""
						##FIXME## QUICK HACK !!!!
						if [ ! "$INTERACTIVE" = "y" ]; then break ; fi
					} >> "$ABCDETEMPDIR/cddblocalchoices"
				done
				if [ $(cat "$ABCDETEMPDIR/cddblocalchoices" | wc -l) -ge 24 ] && [ "$INTERACTIVE" = "y" ]; then
					page "$ABCDETEMPDIR/cddblocalchoices"
				else
					# It's all going to fit in one page, cat it
					cat "$ABCDETEMPDIR/cddblocalchoices" >&2
				fi
				CDDBLOCALCHOICES=$( echo "$CDDBLOCALRESULTS" | wc -l )
				# Setting the choice to an impossible integer to avoid errors in the numeric comparisons
				CDDBLOCALCHOICENUM=-1
				if [ "$INTERACTIVE" = "y" ]; then
					while [ $CDDBLOCALCHOICENUM -lt 0 ] || [ $CDDBLOCALCHOICENUM -gt $CDDBLOCALCHOICES ]; do
						echo -n "Locally cached CDDB entries found. Which one would you like to use (0 for none)? [0-$CDDBLOCALCHOICES]: " >&2
						read CDDBLOCALCHOICE
						[ x"$CDDBLOCALCHOICE" = "x" ] && CDDBLOCALCHOICE="1"
						# FIXME # Introduce diff's
						if echo $CDDBLOCALCHOICE | egrep "[[:space:]]*[[:digit:]]+,[[:digit:]]+[[:space:]]*" > /dev/null 2>&1 ; then
							diffentries cddblocalread "$CDDBLOCALCHOICES" "$CDDBLOCALCHOICE"
						elif echo $CDDBLOCALCHOICE | egrep "[[:space:]]*[[:digit:]]+[[:space:]]*" > /dev/null 2>&1 ; then
							# Make sure we get a valid choice
							CDDBLOCALCHOICENUM=$(echo $CDDBLOCALCHOICE | xargs printf %d 2>/dev/null)
							if [ $CDDBLOCALCHOICENUM -lt 0 ] || [ $CDDBLOCALCHOICENUM -gt $CDDBLOCALCHOICES ]; then
								echo "Invalid selection. Please choose a number between 0 and $CDDBLOCALCHOICES." >&2
							fi
						fi
					done
				else
					### FIXME ###
					#echo "Selected ..."
					CDDBLOCALRESP=y
					CDDBLOCALCHOICENUM=1
				fi
				if [ ! "$CDDBLOCALCHOICENUM" = "0" ]; then
					#echo "Using local copy of CDDB data"
					echo "# DO NOT ERASE THIS LINE! Added by abcde to imitate cddb output" > "$ABCDETEMPDIR/cddbread.1"
					cat "$ABCDETEMPDIR/cddblocalread.$CDDBLOCALCHOICENUM" >> "$ABCDETEMPDIR/cddbread.1"
					echo 999 > "$ABCDETEMPDIR/cddbquery" # Assuming 999 isn't used by CDDB
					echo cddb-readcomplete >> "$ABCDETEMPDIR/status"
					do_cddbparse "$ABCDETEMPDIR/cddbread.1" > "$ABCDETEMPDIR/cddbchoices"
					echo cddb-choice=1 >> "$ABCDETEMPDIR/status"
					CDDBLOCALSTATUS="found"
				else
					#echo "Not using local copy of CDDB data"
					CDDBLOCALSTATUS="notfound"
				fi
				;;
			single)
				# List out disc title/author and contents
				do_cddbparse "${CDDBLOCALFILE}"
				#if [ "$CDROMREADERSYNTAX" = "flac" ] ; then
				#	echo -n "Embedded cuesheet entry found, use it? [y/n] (y): " >&2
				#else
					echo -n "Locally cached CDDB entry found, use it? [y/n] (y): " >&2
				#fi
				if [ "$INTERACTIVE" = "y" ]; then
					read USELOCALRESP
					while [ "$USELOCALRESP" != "y" ] && [ "$USELOCALRESP" != "n" ] && [ "$USELOCALRESP" != "" ] ; do
						echo -n 'Invalid selection. Please answer "y" or "n": ' >&2
						read USELOCALRESP
					done
					[ x"$USELOCALRESP" = "x" ] && USELOCALRESP="y"
				else
					echo "y" >&2
				fi
				if [ "$USELOCALRESP" = "y" ]; then
					#echo "Using local copy of CDDB data"
					echo "# DO NOT ERASE THIS LINE! Added by abcde to imitate cddb output" > "$ABCDETEMPDIR/cddbread.1"
					cat "${CDDBLOCALFILE}" >> "$ABCDETEMPDIR/cddbread.1"
					echo 999 > "$ABCDETEMPDIR/cddbquery" # Assuming 999 isn't used by CDDB
					echo cddb-readcomplete >> "$ABCDETEMPDIR/status"
					do_cddbparse "${CDDBLOCALFILE}" > "$ABCDETEMPDIR/cddbchoices"
					echo cddb-choice=1 >> "$ABCDETEMPDIR/status"
					CDDBLOCALSTATUS="single"
				else
					#echo "Not using local copy of CDDB data"
					CDDBLOCALSTATUS="notfound"
				fi
				;;
			none)
				CDDBLOCALSTATUS="notfound"
				;;
		esac
	fi
}

do_musicbrainzstat ()
{
	:
}

do_musicbrainz ()
{
# Use MBE_TOCGetCDIndexId on a perl query
	:
}

# do_cddbstat
do_cddbstat ()
{
	# Perform CDDB protocol version check if it hasn't already been done
	if checkstatus cddb-statcomplete; then :; else
		if [ "$CDDBAVAIL" = "n" ]; then
			ERRORCODE=no_query
			echo 503 > "$ABCDETEMPDIR/cddbstat"
		else
			rc=1
			CDDBUSER=$(echo $HELLOINFO | cut -f1 -d'@')
			CDDBHOST=$(echo $HELLOINFO | cut -f2- -d'@')
			while test $rc -eq 1 -a $CDDBPROTO -ge 3; do
				vecho "Checking CDDB server status..."
				$CDDBTOOL stat $CDDBURL $CDDBUSER $CDDBHOST $CDDBPROTO > "$ABCDETEMPDIR/cddbstat"
				RESPONSECODE=$(head -n 1 "$ABCDETEMPDIR/cddbstat" | cut -f1 -d' ')
				case "$RESPONSECODE" in
				210)    # 210 OK, status information follows (until terminating `.')
					rc=0;
					;;
				501|*)  # 501 Illegal CDDB protocol level: <n>. 
					CDDBPROTO=`expr $CDDBPROTO - 1`
					;;
				esac 
			done
			if test $rc -eq 1; then
				CDDBAVAIL="n" 
			fi
		fi
		echo cddb-statcomplete >> "$ABCDETEMPDIR/status"
	fi
}


# do_cddbquery
do_cddbquery ()
{
	CDDBDISCID=$(echo $TRACKINFO | cut -d' ' -f1)
	CDDBLOCALFILE="${CDDBLOCALDIR}/${CDDBDISCID}"
	
	# Perform CDDB query if it hasn't already been done
	if checkstatus cddb-querycomplete; then :; else
		if [ "$CDDBAVAIL" = "n" ]; then
			ERRORCODE=no_query
			echo 503 > "$ABCDETEMPDIR/cddbquery"
		# The default CDDBLOCALSTATUS is "notfound"
		# This part will be triggered if the user CDDB repo does not 
		# contain the entry, or if we are not trying to use the repo.
		else
			vecho "Querying the CDDB server..."
			CDDBUSER=$(echo $HELLOINFO | cut -f1 -d'@')
			CDDBHOST=$(echo $HELLOINFO | cut -f2- -d'@')
			$CDDBTOOL query $CDDBURL $CDDBPROTO $CDDBUSER $CDDBHOST $TRACKINFO > "$ABCDETEMPDIR/cddbquery"
			ERRORCODE=$?
			case $ERRORCODE in
				0)  # success
				;;
				12|13|14)
					# no match found in database,
					# wget/fetch error, or user requested not to use CDDB
					# Make up an error code (503) that abcde
					# will recognize in do_cddbread
					# and compensate by making a template
					echo 503 > "$ABCDETEMPDIR/cddbquery"
				;;
				*) # strange and unknown error
					echo ERRORCODE=$ERRORCODE
					echo "abcde: $CDDBTOOL returned unknown error code"
				;;
			esac
		fi
		echo cddb-querycomplete >> "$ABCDETEMPDIR/status"
	fi
}

# do_cddbread
do_cddbread ()
{
	# If it's not to be used, generate a template.
	# Then, display it (or them) and let the user choose/edit it
	if checkstatus cddb-readcomplete; then :; else
		vecho "Obtaining CDDB results..."
		# If CDDB is to be used, interpret the query results and read all
		# the available entries.
		rm -f "$ABCDETEMPDIR/cddbchoices"
		CDDBCHOICES=1 # Overridden by multiple matches
		RESPONSECODE=$(head -n 1 "$ABCDETEMPDIR/cddbquery" | cut -f1 -d' ')
		case "$RESPONSECODE" in
		200)
			# One exact match, retrieve it
			# 200 [section] [discid] [artist] / [title]
			if checkstatus cddb-read-1-complete; then :; else
				echo -n "Retrieving 1 CDDB match..." >> "$ABCDETEMPDIR/cddbchoices"
				$CDDBTOOL read $CDDBURL $CDDBPROTO $CDDBUSER $CDDBHOST $(cut -f2,3 -d' ' "$ABCDETEMPDIR/cddbquery") > "$ABCDETEMPDIR/cddbread.1"
				echo "done." >> "$ABCDETEMPDIR/cddbchoices"
				echo cddb-read-1-complete >> "$ABCDETEMPDIR/status"
				echo cddb-choice=1 >> "$ABCDETEMPDIR/status"
			fi
			# List out disc title/author and contents
			echo ---- "$(cut '-d ' -f4- "$ABCDETEMPDIR/cddbquery")" ---- >> "$ABCDETEMPDIR/cddbchoices"
			for TRACK in $(f_seq_row 1 $TRACKS)
			do
				echo $TRACK: "$(grep ^TTITLE$(expr $TRACK - 1)= "$ABCDETEMPDIR/cddbread.1" | cut -f2- -d= | tr -d \\r\\n)" >> "$ABCDETEMPDIR/cddbchoices"
			done
			echo >> "$ABCDETEMPDIR/cddbchoices"
			;;
		202|403|409|503)
			# No match
			case "$RESPONSECODE" in
			202) echo "No CDDB match." >> "$ABCDETEMPDIR/cddbchoices" ;;
			403|409) echo "CDDB entry is corrupt, or the handshake failed." >> "$ABCDETEMPDIR/cddbchoices" ;;
			503) echo "CDDB unavailable." >> "$ABCDETEMPDIR/cddbchoices" ;;
			esac
			$CDDBTOOL template $(cat "$ABCDETEMPDIR/discid") > "$ABCDETEMPDIR/cddbread.0"
			# List out disc title/author and contents of template
			echo ---- Unknown Artist / Unknown Album ---- >> "$ABCDETEMPDIR/cddbchoices"
			UNKNOWNDISK=y
			for TRACK in $(f_seq_row 1 $TRACKS)
			do
				echo $TRACK: "$(grep ^TTITLE$(expr $TRACK - 1)= "$ABCDETEMPDIR/cddbread.0" | cut -f2- -d= | tr -d \\r\\n)" >> "$ABCDETEMPDIR/cddbchoices"
			done
			echo >> "$ABCDETEMPDIR/cddbchoices"
			echo cddb-read-0-complete >> "$ABCDETEMPDIR/status"
			echo cddb-choice=0 >> "$ABCDETEMPDIR/status"
			;;
		210|211)
			# Multiple exact, (possibly multiple) inexact matches
			IN=
			if [ "$RESPONSECODE" = "211" ]; then IN=in; fi
			if [ "$(wc -l < "$ABCDETEMPDIR/cddbquery" | tr -d ' ')" -eq 3 ]; then
				echo "One ${IN}exact match:" >> "$ABCDETEMPDIR/cddbchoices"
				tail -n +2 "$ABCDETEMPDIR/cddbquery" | head -n 1 >> "$ABCDETEMPDIR/cddbchoices"
	                        echo cddb-choice=1 >> "$ABCDETEMPDIR/status"
			else
				echo "Multiple ${IN}exact matches:" >> "$ABCDETEMPDIR/cddbchoices"
			fi
			vecho -n "Retrieving multiple matches... "
			grep -v ^[.]$ "$ABCDETEMPDIR/cddbquery" | ( X=0
			read DISCINFO # eat top line
			while read DISCINFO
			do
				X=$(expr $X + 1)
				if checkstatus cddb-read-$X-complete; then :; else
					$CDDBTOOL read $CDDBURL $CDDBPROTO $CDDBUSER $CDDBHOST $(echo $DISCINFO | cut -f1,2 -d' ') > "$ABCDETEMPDIR/cddbread.$X"
					echo cddb-read-$X-complete >> "$ABCDETEMPDIR/status"
				fi
				# List out disc title/author and contents
				echo \#$X: ---- "$DISCINFO" ---- >> "$ABCDETEMPDIR/cddbchoices"
				for TRACK in $(f_seq_row 1 $TRACKS)
				do
					echo $TRACK: "$(grep ^TTITLE$(expr $TRACK - 1)= "$ABCDETEMPDIR/cddbread.$X" | cut -f2- -d= | tr -d \\r\\n)" >> "$ABCDETEMPDIR/cddbchoices"
				done
				echo >> "$ABCDETEMPDIR/cddbchoices"
			done )
			vecho "done."
			CDDBCHOICES=$(expr $(cat "$ABCDETEMPDIR/cddbquery" | wc -l) - 2)
			;;
		999)
			# Using local copy.
			for TRACK in $(f_seq_row 1 $TRACKS)
			do
				echo $TRACK: "$(grep ^TTITLE$(expr $TRACK - 1)= "$ABCDETEMPDIR/cddbread.1" | cut -f2- -d= | tr -d \\r\\n)" >> "$ABCDETEMPDIR/cddbchoices"
			done
			echo >> "$ABCDETEMPDIR/cddbchoices"
			echo cddb-read-1-complete >> "$ABCDETEMPDIR/status"
			echo cddb-choice=1 >> "$ABCDETEMPDIR/status"
			;;
		esac	
		echo "cddb-readcomplete" >> "$ABCDETEMPDIR/status"
	fi
}

# do_cddbedit
do_cddbedit ()
{
	if checkstatus cddb-edit >/dev/null; then
		CDDBDATA="$ABCDETEMPDIR/cddbread.$(checkstatus cddb-choice)"
		VARIOUSARTISTS="$(checkstatus variousartists)"
		VARIOUSARTISTSTYLE="$(checkstatus variousartiststyle)"
		return 0
	fi
	if [ "$INTERACTIVE" = "y" ]; then
		# We should show the CDDB results both when we are not using the local CDDB repo
		# or when we are using it but we could not find a proper match
		if [ "$CDDBUSELOCAL" = "y" ] && [ "$CDDBLOCALSTATUS" = "notfound" ] || [ ! "$CDDBUSELOCAL" = "y" ]; then
			# Display the $ABCDETEMPDIR/cddbchoices file created above
			# Pick a pager so that if the tracks overflow the screen the user can still view everything
			if [ -r "$ABCDETEMPDIR/cddbchoices" ]; then
				CDDBCHOICES=$(expr $(cat "$ABCDETEMPDIR/cddbquery" | wc -l) - 2)
				CHOICE=$(checkstatus cddb-choice)
				if [ -n "$CHOICE" ] ; then
					case $CDDBCHOICES in
						-1) if head -1 "$ABCDETEMPDIR/cddbquery" | grep "^$" > /dev/null 2>&1 ; then
								log error "CDDB query failed!" 
								exit 1
							else
								cat "$ABCDETEMPDIR/cddbchoices"
							fi
							;;
						1) cat "$ABCDETEMPDIR/cddbchoices" ;;
						*)
						echo "Selected: #$CHOICE"
						do_cddbparse "$ABCDETEMPDIR/cddbread.$CHOICE"
						;;
					esac
				else
					# The user has a choice to make, display the info in a pager if necessary
					if [ $(cat "$ABCDETEMPDIR/cddbchoices" | wc -l) -ge 24 ]; then
						page "$ABCDETEMPDIR/cddbchoices"
					else
						# It's all going to fit in one page, cat it
						cat "$ABCDETEMPDIR/cddbchoices" >&2
					fi
					
					CDDBCHOICENUM=""
					# Setting the choice to an impossible integer to avoid errors in the numeric comparisons
					CDCHOICENUM=-1
					# I'll take CDDB read #3 for $400, Alex
					while [ $CDCHOICENUM -lt 0 ] || [ $CDCHOICENUM -gt $CDDBCHOICES ]; do
						echo -n "Which entry would you like abcde to use (0 for none)? [0-$CDDBCHOICES]: " >&2
						read CDDBCHOICE
						[ X"$CDDBCHOICE" = "X" ] && CDDBCHOICE=1
						if echo $CDDBCHOICE | egrep "[[:space:]]*[[:digit:]]+,[[:digit:]]+[[:space:]]*" > /dev/null 2>&1 ; then
							if [ ! X"$DIFF" = "X" ]; then
								PARSECHOICE1=$(echo $CDDBCHOICE | cut -d"," -f1 | xargs printf %d 2>/dev/null)
								PARSECHOICE2=$(echo $CDDBCHOICE | cut -d"," -f2 | xargs printf %d 2>/dev/null)
								if [ $PARSECHOICE1 -lt 1 ] || [ $PARSECHOICE1 -gt $CDDBCHOICES ] || \
								   [ $PARSECHOICE2 -lt 1 ] || [ $PARSECHOICE2 -gt $CDDBCHOICES ] || \
								   [ $PARSECHOICE1 -eq $PARSECHOICE2 ]; then 
									echo "Invalid diff range. Please select two coma-separated numbers between 1 and $CDDBCHOICES" >&2
								else
									# We parse the 2 choices to diff, store them in temporary files and diff them.
									for PARSECHOICE in $(echo $CDDBCHOICE | tr , \ ); do
										do_cddbparse "$ABCDETEMPDIR/cddbread.$PARSECHOICE" > "$ABCDETEMPDIR/cddbread.parsechoice.$PARSECHOICE"
									done
									echo "Showing diff between choices $PARSECHOICE1 and $PARSECHOICE2..." > "$ABCDETEMPDIR/cddbread.diff"
									$DIFF $DIFFOPTS "$ABCDETEMPDIR/cddbread.parsechoice.$PARSECHOICE1" "$ABCDETEMPDIR/cddbread.parsechoice.$PARSECHOICE2" >> "$ABCDETEMPDIR/cddbread.diff"
									if [ $(cat "$ABCDETEMPDIR/cddbread.diff" | wc -l) -ge 24 ]; then
										page "$ABCDETEMPDIR/cddbread.diff"
									else
										cat "$ABCDETEMPDIR/cddbread.diff" >&2
									fi
								fi
							else
								echo "The diff program was not found in your path. Please choose a number between 0 and $CDDBCHOICES." >&2
							fi
						elif echo $CDDBCHOICE | egrep "[[:space:]]*[[:digit:]]+[[:space:]]*" > /dev/null 2>&1 ; then
							# Make sure we get a valid choice
							CDCHOICENUM=$(echo $CDDBCHOICE | xargs printf %d 2>/dev/null)
							if [ $CDCHOICENUM -lt 0 ] || [ $CDCHOICENUM -gt $CDDBCHOICES ]; then
								echo "Invalid selection. Please choose a number between 0 and $CDDBCHOICES." >&2
							fi
						fi
					done
					if [ "$CDCHOICENUM" = "0" ]; then
						vecho "Creating empty CDDB template..."
						UNKNOWNDISK=y
						$CDDBTOOL template $(cat "$ABCDETEMPDIR/discid") > "$ABCDETEMPDIR/cddbread.0"
					else
						echo "Selected: #$CDCHOICENUM ($(grep ^DTITLE= "$ABCDETEMPDIR/cddbread.$CDCHOICENUM" | cut -f2- -d= | tr -d \\r\\n))" >&2
						do_cddbparse "$ABCDETEMPDIR/cddbread.$CDCHOICENUM"
					fi
					echo "cddb-choice=$CDCHOICENUM" >> "$ABCDETEMPDIR/status"
				fi
			fi
		else
			# We need some code to show the selected option when local repository is selected and we have found a match
			vecho "Using cached CDDB match..." >&2
			# Display the $ABCDETEMPDIR/cddbchoices file created above
			# Pick a pager so that if the tracks overflow the screen the user can still view everything
			if [ -r "$ABCDETEMPDIR/cddbchoices" ]; then
				CDDBCHOICES=$(expr $(cat "$ABCDETEMPDIR/cddbquery" | wc -l) - 2)
				CHOICE=$(checkstatus cddb-choice)
				if [ "$USELOCALRESP" = "y" ]; then :; else
					if [ -n "$CHOICE" ] ; then
						case $CDDBCHOICES in
							0) 
							UNKNOWNDISK=y
							echo "Selected template."
							;;
							1) cat "$ABCDETEMPDIR/cddbchoices" ;;
							*)
							echo "Selected: #$CHOICE"
							do_cddbparse "$ABCDETEMPDIR/cddbread.$CHOICE"
							;;
						esac
					fi
				fi
			fi
		fi
	else
		# We're noninteractive - pick the first choice.
		# But in case we run a previous instance and selected a choice, use it.
		if [ -r "$ABCDETEMPDIR/cddbchoices" ]; then
			# Show the choice if we are not using the locally stored one
			# or when the local search failed to find a match.
			PREVIOUSCHOICE=$(checkstatus cddb-choice)
			if [ "$CDDBUSELOCAL" = "y" ] && [ "$CDDBLOCALSTATUS" = "notfound" ] || [ ! "$CDDBUSELOCAL" = "y" ]; then
				#if [ "$PREVIOUSCHOICE" ]; then
					cat "$ABCDETEMPDIR/cddbchoices"
				#fi
			fi
			if [ ! -z "$PREVIOUSCHOICE" ] ; then
				CDCHOICENUM=$PREVIOUSCHOICE
			else
				CDCHOICENUM=1
				echo "cddb-choice=$CDCHOICENUM" >> "$ABCDETEMPDIR/status"
			fi
			echo "Selected: #$CDCHOICENUM ($(grep ^DTITLE= "$ABCDETEMPDIR/cddbread.$CDCHOICENUM" | cut -f2- -d= | tr -d \\r\\n))" >&2
		fi
	fi

	# sanity check
	if checkstatus cddb-choice >/dev/null; then :; else
		echo "abcde: internal error: cddb-choice not recorded." >&2
		exit 1
	fi
	CDDBDATA="$ABCDETEMPDIR/cddbread.$(checkstatus cddb-choice)"
	echo -n "Edit selected CDDB data? [y/n] (" >&2
	if [ "$INTERACTIVE" = "y" ]; then
		if [ "$UNKNOWNDISK" = "y" ]; then
			echo -n "y): " >&2
			read EDITCDDB
			[ "$EDITCDDB" != "n" ] && EDITCDDB=y
		else
			echo -n "n): " >&2
			read EDITCDDB
		fi
	else
		echo "n): n" >&2
		EDITCDDB=n
	fi
	if [ "$EDITCDDB" = "y" ]; then
		CDDBDATAMD5SUM=$($MD5SUM "$CDDBDATA" | cut -d " " -f 1);
		
		# Use the debian sensible-editor wrapper to pick the editor that the
		# user has requested via their $EDITOR environment variable
		if [ -x "/usr/bin/sensible-editor" ]; then
			/usr/bin/sensible-editor "$CDDBDATA"
		elif [ -n "$EDITOR" ]; then
			if [ -x $(which "${EDITOR%%\ *}") ]; then
				# That failed, try to load the preferred editor, starting
				# with their EDITOR variable
				eval $(echo "$EDITOR") \"$CDDBDATA\"
			fi
		# If that fails, check for a vi
		elif which vi >/dev/null 2>&1; then
			vi "$CDDBDATA"
		elif [ -x /usr/bin/vim ]; then
			/usr/bin/vim "$CDDBDATA"
		elif [ -x /usr/bin/vi ]; then
			/usr/bin/vi "$CDDBDATA"
		elif [ -x /bin/vi ]; then
			/bin/vi "$CDDBDATA"
		# nano should be on all (modern, i.e., sarge) debian systems
		elif which nano >/dev/null 2>&1 ; then
			nano "$CDDBDATA"
		elif [ -x /usr/bin/nano ]; then
			/usr/bin/nano "$CDDBDATA"
		# mg should be on all OpenBSD systems
		elif which mg >/dev/null 2>&1 ; then
			mg "$CDDBDATA"
		elif [ -x /usr/bin/mg ]; then
			/usr/bin/mg "$CDDBDATA"
		# bomb out
		else
			log warning "no editor available. Check your EDITOR environment variable."
		fi
		# delete editor backup file if it exists
		if [ -w "$CDDBDATA~" ]; then
			rm -f "$CDDBDATA~"
		fi
	fi

	# Some heuristics first. Look at Disc Title, and if it starts with
	# "Various", then we'll assume Various Artists
	if [ "$(grep ^DTITLE= "$CDDBDATA" | cut -f2- -d= | egrep -ci '^(various|soundtrack|varios|sonora|ost)')" != "0" ]; then
		echo "Looks like a Multi-Artist CD" >&2
		VARIOUSARTISTS=y
	else
		echo -n "Is the CD multi-artist? [y/n] (n): " >&2
		if [ "$INTERACTIVE" = "y" ]; then
			read VARIOUSARTISTS
		else
			echo n >&2
			VARIOUSARTISTS=n
		fi
	fi
	if [ "$VARIOUSARTISTS" = "y" ] && [ ! "$ONETRACK" = "y" ]; then
		# Set a default
		DEFAULTSTYLE=1
		# Need NUMTRACKS before cddb-tool will return it:
		NUMTRACKS=$(egrep '^TTITLE[0-9]+=' "$CDDBDATA" | wc -l)
		if [ "$(grep -c "^TTITLE.*\/" "$CDDBDATA")" -gt "$(expr $NUMTRACKS / 2 )" ]; then
			# More than 1/2 tracks contain a "/", so guess forward
			DEFAULTSTYLE=1
		elif [ "$(grep -c "^TTITLE.*\-" "$CDDBDATA")" -gt "$(expr $NUMTRACKS / 2 )" ]; then
			# More than 1/2 contain a "-", so guess forward-dash
			DEFAULTSTYLE=2
		elif [ "$(grep -c "^TTITLE.*(.*)" "$CDDBDATA")" -gt "$(expr $NUMTRACKS / 2 )" ]; then
			# More than 1/2 contain something in parens, so guess trailing-paren
			DEFAULTSTYLE=6
		fi

		echo "1) Artist / Title" >&2
		echo "2) Artist - Title" >&2
		echo "3) Title / Artist" >&2
		echo "4) Title - Artist" >&2
		echo "5) Artist: Title" >&2
		echo "6) Title (Artist)" >&2
		echo "7) This is a single-artist CD" >&2
		echo -n "Which style of multiple artist entries is it? [1-7] ($DEFAULTSTYLE): " >&2
		if [ "$INTERACTIVE" = "y" ]; then
			read VARIOUSARTISTSTYLE
		else
			echo $DEFAULTSTYLE >&2
			VARIOUSARTISTSTYLE=$DEFAULTSTYLE
		fi
		VARIOUSARTISTSTYLE=$(echo 0$VARIOUSARTISTSTYLE | xargs printf %d)
		# If they press Enter, then the default style (0) was chosen
		while [ $VARIOUSARTISTSTYLE -lt 0 ] || [ $VARIOUSARTISTSTYLE -gt 7 ]; do
			echo "Invalid selection. Please choose a number between 1 and 7."
			echo -n "Selection [1-7]: "
			read VARIOUSARTISTSTYLE
			VARIOUSARTISTSTYLE=$(echo 0$VARIOUSARTISTSTYLE | xargs printf %d)
		done
		if [ "$VARIOUSARTISTSTYLE" = "0" ]; then
			VARIOUSARTISTSTYLE=$DEFAULTSTYLE
		fi
		vecho "Selected: $VARIOUSARTISTSTYLE"
		case "$VARIOUSARTISTSTYLE" in
		1) # Artist / Title
			VARIOUSARTISTSTYLE=forward
			;;
		2) # Artist - Title
			VARIOUSARTISTSTYLE=forward-dash
			;;
		3) # Title / Artist
			VARIOUSARTISTSTYLE=reverse
			;;
		4) # Title - Artist
			VARIOUSARTISTSTYLE=reverse-dash
			;;
		5) # Artist: Title
			VARIOUSARTISTSTYLE=colon
			;;
		6) # Title (Artist)
			VARIOUSARTISTSTYLE=trailing-paren
			;;
		7) # Single Artist
			VARIOUSARTISTS=n
			;;
		esac
	fi

	echo "variousartists=$VARIOUSARTISTS" >> "$ABCDETEMPDIR/status"
	echo "variousartiststyle=$VARIOUSARTISTSTYLE" >> "$ABCDETEMPDIR/status"

	if [ "$EDITCDDB" = "y" ] && [ "$UNINTENTIONALLY_ANGER_THE_FREEDB_PEOPLE" = "y" ]; then
		if [ "$CDDBDATAMD5SUM" != "" ]  && [ "$CDDBDATAMD5SUM" != "$($MD5SUM "$CDDBDATA" | cut -d " " -f 1)" ]; then
			# This works but does not have the necessary error checking
			# yet. If you are familiar with the CDDB spec
			# (see http://www.freedb.org/src/latest/DBFORMAT) 
			# and can create an error-free entry on your own, then put
			# UNINTENTIONALLY_ANGER_THE_FREEDB_PEOPLE=y in your
			# abcde.conf to enable it. Put CDDBSUBMIT=email@address in
			# your abcde.conf to change the email address submissions are
			# sent to.

			# submit the modified file, if they want
			if [ "$NOSUBMIT" != "y" ]; then
				echo -n "Do you want to submit this entry to $CDDBSUBMIT? [y/n] (n): "
				read YESNO
				while [ "$YESNO" != "y" ] && [ "$YESNO" != "n" ] && [ "$YESNO" != "Y" ] && \
					[ "$YESNO" != "N" ] && [ "$YESNO" != "" ]
				do
					echo -n 'Invalid selection. Please answer "y" or "n": '
					read YESNO
				done
				if [ "$YESNO" = "y" ] || [ "$YESNO" = "Y" ]; then
					echo -n "Sending..."
					$CDDBTOOL send "$CDDBDATA" $CDDBSUBMIT
					echo "done."
				fi
			fi
		fi
	fi
	### FIXME ###
	# User CDDBLOCALPOLICY to find out if we store the file or not...
	# Cache edited CDDB entry in the user's cddb dir
	if [ "$CDDBCOPYLOCAL" = "y" ]; then
		# Make sure the cache directory exists
		mkdir -p $CDDBLOCALDIR
		cat "$CDDBDATA" | tail -n $(expr $(cat "$CDDBDATA" | wc -l ) - 1 ) > ${CDDBLOCALDIR}/$(echo "$TRACKINFO" | cut -d' ' -f1)
	fi

	echo "cddb-edit" >> "$ABCDETEMPDIR/status"
}

# do_cdread [tracknumber]
# do_cdread onetrack [firsttrack] [lasttrack]
# 
do_cdread ()
{
	# The commands here don't go through run_command because they're never supposed to be silenced
	# return codes need to be doublechecked anyway, however
	if [ "$1" = "onetrack" ]; then
		# FIXME # Add the possibility of grabbing ranges of tracks in onetrack
		# FIXME # Until then, we grab the whole CD in one track, no matter what
		# the user said
		# We need the first and last track for cdda2wav
		FIRSTTRACK=$2
		LASTTRACK=$(expr $3 + 0)
		UTRACKNUM=$FIRSTTRACK
		case "$CDROMREADERSYNTAX" in
			flac) READTRACKNUMS="$FIRSTTRACK.1-$(($LASTTRACK + 1)).0" ;;
			cdparanoia) 
				#XX FIXME XX
				# Add a variable to check if tracks are provided in command line and if not, use "0-" to rip the tracks
				READTRACKNUMS="$FIRSTTRACK-$LASTTRACK" ;;
			cdda2wav) READTRACKNUMS="$FIRSTTRACK+$LASTTRACK" ;;
			*) echo "abcde error: $CDROMREADERSYNTAX does not support ONETRACK mode"
				exit 1 ;;
		esac
	else
		UTRACKNUM=$1
	fi
	CDDBTRACKNUM=$(expr $UTRACKNUM - 1)
	if [ "$USEPIPES" = "y" ]; then
		TEMPARG="PIPERIPPER_$CDROMREADERSYNTAX"
		FILEARG="$( eval echo "\$$TEMPARG" )"
		REDIR=""
		PIPE_MESSAGE="and encoding "
	else
		WAVDATA="$ABCDETEMPDIR/track$UTRACKNUM.wav"
		case "$CDROMREADERSYNTAX" in
		## FIXME ## Find the cases for dagrab and flac, to avoid exceptions
			flac)
				FILEARG="--output-name=$WAVDATA"
				;;
			dagrab)
				FILEARG="-f $WAVDATA"
				;;
			*)
				FILEARG="$WAVDATA"
				;;
		esac
		REDIR=">&2"
	fi
	if [ "$1" = "onetrack" ]; then
		echo "Grabbing ${PIPE_MESSAGE}tracks $UTRACKNUM - $LASTTRACK as one track ..." >&2
	else
		if [ -r "$CDDBDATA" ]; then
			getcddbinfo TRACKNAME
			echo "Grabbing ${PIPE_MESSAGE}track $UTRACKNUM: $TRACKNAME..." >&2
		else
			echo "Grabbing ${PIPE_MESSAGE}track $UTRACKNUM..." >&2
		fi
	fi
	case "$CDROMREADERSYNTAX" in
		### FIXME ### use an exception for flac, since it uses -o
		### FIXME ### Shall we just use -o $FILEARG ??
		flac)
			# Avoid problems wit math expressions by unpadding the given UTRACKNUM
			STRIPTRACKNUM=$(expr $UTRACKNUM + 0)
			nice $READNICE $FLAC -d -f --cue=${READTRACKNUMS:-$STRIPTRACKNUM.1-$(($STRIPTRACKNUM + 1)).0} "$FILEARG" "$CDROM" ;;
		cdparanoia) 
			nice $READNICE $CDROMREADER -$CDPARANOIACDROMBUS "$CDROM" ${READTRACKNUMS:-$UTRACKNUM} "$FILEARG" $REDIR ;;
		cdda2wav)
			if [ "$OSFLAVOUR" = "OSX" ] ; then
				# Hei, we have to unmount the device before running anything like cdda2wav in OSX
				disktool -u ${CDROM#/dev/} 0
				# Also, in OSX the cdrom device for cdda2wav changes...
				CDDA2WAVCDROM="IODVDServices"
			elif [ "$OSFLAVOUR" = "FBSD" ] ; then
				CDDA2WAVCDROM="$CDROMID"
			else
				if [ "$CDROMID" = "" ]; then
					CDDA2WAVCDROM="$CDROM"
				else
					CDDA2WAVCDROM="$CDROMID"
				fi
			fi
			nice $READNICE $CDROMREADER -D $CDDA2WAVCDROM -t ${READTRACKNUMS:-$UTRACKNUM} "$FILEARG" $REDIR
			;;
		## FIXME ## We have an exception for dagrab, since it uses -f
		## FIXME ## Shall we just use -f $FILEARG ??
		dagrab) nice $READNICE $CDROMREADER -d "$CDROM" -v $UTRACKNUM "$FILEARG" $REDIR
			;;
		cddafs)
			# Find the track's mounted path
			REALTRACKNUM=$(expr $UTRACKNUM + 0)
			FILEPATH=$(mount | grep "$CDROM on" | sed 's/^[^ ]* on \(.*\) (.*/\1/')
			FILEPATH=$(find "$FILEPATH" | grep "/$REALTRACKNUM ");
			# If the file exists, copy it
			if [ -e "$FILEPATH" ] ; then
				nice $READNICE $CDROMREADER "$FILEPATH" "$FILEARG" $REDIR
			else
				false
			fi ;;
		debug) nice $READNICE $CDROMREADER -$CDPARANOIACDROMBUS "$CDROM" -w $UTRACKNUM-[:1] "$FILEARG" $REDIR
			;;
	esac
	RETURN=$?
	# If we get some error or we get some missing wav 
	# (as long as we dont use pipes)
	if [ "$RETURN" != "0" -o \( ! -s "$WAVDATA" -a X"$USEPIPES" != "Xy" \) ]; then
		# Thank goodness errors is only machine-parseable up to the
		# first colon, otherwise this woulda sucked
		if [ "$RETURN" = "0" -a ! -s "$WAVDATA" ]; then
			RETURN=73 # fake a return code as cdparanoia return 0 also on aborted reads
		fi
		if [ "$USEPIPES" = "y" ]; then
			echo "readencodetrack-$UTRACKNUM: $CDROMREADER returned code $RETURN" >> "$ABCDETEMPDIR/errors"
		else
			echo "readtrack-$UTRACKNUM: $CDROMREADER returned code $RETURN" >> "$ABCDETEMPDIR/errors"
		fi
		return $RETURN
	else
		if [ "$USEPIPES" = "y" ]; then
			echo readencodetrack-$UTRACKNUM >> "$ABCDETEMPDIR/status"
		else
			echo readtrack-$UTRACKNUM >> "$ABCDETEMPDIR/status"
		fi
		if [ "$1" = "onetrack" ]; then
			echo onetrack >> "$ABCDETEMPDIR/status"
		fi
	fi
}

# do_cdspeed
# No values accepted, only uses env variables
do_cdspeed () 
{
	if "$CDSPEED" "$CDSPEEDOPTS" "$CDSPEEDVALUE" >/dev/null ; then
		vecho "Setting CD speed to ${CDSPEEDVALUE}x"
	else
		echo "abcde: unable to set the device speed" >&2
	fi
}

# vecho [message]
#
# vecho outputs a message if EXTRAVERBOSE is selected
vecho ()
{
if [ x"$EXTRAVERBOSE" != "x" ]; then
	case $1 in
		warning) shift ; log warning "$@" ;;
		*) echo "$@" ;;
	esac
fi
}

# decho [message]
#
# decho outputs a debug message if DEBUG is selected
decho ()
{
if [ x"$DEBUG" != "x" ]; then
	if echo $1 | grep "^\[" > /dev/null 2>&1 ; then
		DEBUGECHO=$(echo "$@" | tr -d '[]')
		echo "[DEBUG] $DEBUGECHO: `eval echo \\$${DEBUGECHO}`"
	else
		echo "[DEBUG] $1"
	fi
fi
}

# User-redefinable functions
# Custom filename munging:
mungefilename ()
{
	#echo "$@" | sed s,:,\ -,g | tr \ /\* __+ | tr -d \'\"\?\[:cntrl:\]
	echo "$@" | sed s,:,\ -,g | tr \ / __ | tr -d \'\"\?\[:cntrl:\]
}

# Custom genre munging:
mungegenre ()
{
	echo $CDGENRE | tr "[:upper:]" "[:lower:]"
}

# pre_read
# Empty pre_read function, to be defined in the configuration file.
pre_read ()
{
:
}

# post_read
# Empty post_read function, to be defined in the configuration file.
post_read ()
{
:
}

###############################################################################
# End of functions
#
# Start of execution
###############################################################################

# Builtin defaults

# CDDB
# Defaults to FreeDB, but a python musicbrainz can be used
CDDBMETHOD=cddb
CDDBURL="http://freedb.freedb.org/~cddb/cddb.cgi"
CDDBSUBMIT=freedb-submit@freedb.org
CDDBPROTO=6
HELLOINFO="$(whoami)@$(hostname)"
CDDBCOPYLOCAL="n"
CDDBLOCALPOLICY="always"
CDDBLOCALRECURSIVE="y"
CDDBLOCALDIR="$HOME/.cddb"
CDDBUSELOCAL="n"

# List of fields we parse and show during the CDDB parsing...
SHOWCDDBFIELDS="year,genre"

INTERACTIVE=y
#CDROMREADERSYNTAX=cdparanoia
ENCODERSYNTAX=default

MP3ENCODERSYNTAX=default
OGGENCODERSYNTAX=default
FLACENCODERSYNTAX=default
SPEEXENCODERSYNTAX=default
MPPENCODERSYNTAX=default
AACENCODERSYNTAX=default
NORMALIZERSYNTAX=default
CUEREADERSYNTAX=default

OUTPUTFORMAT='${ARTISTFILE}-${ALBUMFILE}/${TRACKNUM}.${TRACKFILE}'
# Use the following VAOUTPUTFORMAT to revert to 2.0.x VA format:
#VAOUTPUTFORMAT=${OUTPUTFORMAT}
VAOUTPUTFORMAT='Various-${ALBUMFILE}/${TRACKNUM}.${ARTISTFILE}-${TRACKFILE}'
ONETRACKOUTPUTFORMAT='${ARTISTFILE}-${ALBUMFILE}/${ALBUMFILE}'
VAONETRACKOUTPUTFORMAT='Various-${ALBUMFILE}/${ALBUMFILE}'
PLAYLISTFORMAT='${ARTISTFILE}-${ALBUMFILE}.${OUTPUT}.m3u'
PLAYLISTDATAPREFIX=''
VAPLAYLISTFORMAT='${ARTISTFILE}-${ALBUMFILE}.${OUTPUT}.m3u'
VAPLAYLISTDATAPREFIX=''
DOSPLAYLIST=n
COMMENT=''
ID3TAGV=2
ENCNICE=10
READNICE=10
DISTMP3NICE=10
VARIOUSARTISTS=n
VARIOUSARTISTSTYLE=forward
KEEPWAVS=n
PADTRACKS=n
NOGAP=n
BATCHNORM=n
NOCDDBQUERY=n

# If using scsi devices, cdda2wav needs a CDROMID, instead of a device node
# i.e. CDROMID="1,0,0"
CDROMID=""
# If we are using the IDE bus, we need CDPARANOIACDROMBUS defined as "d"
# If we are using the ide-scsi emulation layer, we need to define a "g"
CDPARANOIACDROMBUS="d"

# program paths - defaults to checking your $PATH
# mp3
LAME=lame
TOOLAME=toolame
GOGO=gogo
BLADEENC=bladeenc
L3ENC=l3enc
XINGMP3ENC=xingmp3enc
MP3ENC=mp3enc
# ogg
VORBIZE=vorbize
OGGENC=oggenc
# flac
FLAC=flac
# speex
SPEEXENC=speexenc
# mpp (Musepack)
MPPENC=mppenc
# m4a
AACENC=faac

ID3=id3
ID3V2=id3v2
EYED3=eyeD3
CDPARANOIA=cdparanoia
CDDA2WAV=cdda2wav
DAGRAB=dagrab
CDDAFS=cp
CDDISCID=cd-discid
CDDBTOOL=cddb-tool
MUSICBRAINZ=musicbrainz-get-tracks
EJECT=eject
MD5SUM=md5sum
DISTMP3=distmp3
VORBISCOMMENT=vorbiscomment
METAFLAC=metaflac
NORMALIZE=normalize-audio
CDSPEED=eject
VORBISGAIN=vorbisgain
MP3GAIN=mp3gain
MPPGAIN=replaygain
MKCUE=mkcue
MKTOC=cdrdao
DIFF=diff
CUE2DISCID=builtin

# Options for programs called from abcde
# mp3
LAMEOPTS=
TOOLAMEOPTS=
GOGOOPTS=
BLADEENCOPTS=
L3ENCOPTS=
XINGMP3ENCOPTS=
MP3ENCOPTS=
# ogg
VORBIZEOPTS=
OGGENCOPTS=
# flac
FLACOPTS=
# speex
SPEEXENCOPTS=
# mpc
MPPENCOPTS=
# m4a
AACENCOPTS=

ID3OPTS=
ID3V2OPTS=
CDPARANOIAOPTS=
CDDA2WAVOPTS=
DAGRABOPTS=
CDDAFSOPTS="-f"
CDDBTOOLOPTS=
EJECTOPTS=
DISTMP3OPTS=
NORMALIZEOPTS=
CDSPEEDOPTS="-x"
CDSPEEDVALUE=
MKCUEOPTS=
MKTOCOPTS=""
VORBISCOMMENTOPTS="-R"
METAFLACOPTS="--no-utf8-convert"
DIFFOPTS=

# Default to one process if -j isn't specified
MAXPROCS=1

# List of actions to perform - by default, run to completion
ACTIONS=cddb,read,encode,tag,move,replaygain,clean

# This option is basicaly for Debian package dependencies: 
# List of prefered outputs - by default, run with whatever we have in the path
DEFAULT_OUTPUT_BINARIES=vorbis:oggenc,flac:flac,mp3:toolame,mp3:lame,mp3:bladeenc,spx:speex,m4a:faac

# List of prefered cdromreaders - by default, run whichever we have in the path
DEFAULT_CDROMREADERS="cdparanoia cdda2wav"

# Asume fetch if under FreeBSD. curl is used for Mac OS X. wget is used for
# Linux/OpenBSD. ftp is user for NetBSD.
# Let's use these checkings to determine the OS flavour, which will be used
# later
if [ X$(uname) = "XFreeBSD" ] ; then
	HTTPGET=fetch
	MD5SUM=md5
	NEEDCDROMID=y
	OSFLAVOUR=FBSD
elif [ X$(uname) = "XDarwin" ] ; then
	HTTPGET=curl
	OSFLAVOUR=OSX
	# We should have disktool in OSX, but let's be sure...
	NEEDDISKTOOL=y
	CDROMREADERSYNTAX=cddafs
elif [ X$(uname) = "XOpenBSD" ] ; then
	HTTPGET=wget
	MD5SUM=md5
	OSFLAVOUR=OBSD
elif [ X$(uname) = "XNetBSD" ] ; then
	HTTPGET=ftp
	MD5SUM=md5
	OSFLAVOUR=NBSD
elif [ X$(uname) = "SunOS" ] ; then
	HTTPGET=""
	MD5SUM=md5
	OSFLAVOUR=SunOS
else
	HTTPGET=wget
fi

# If CDDBAVAIL is set to n, no CDDB read is done
# If USEID3 is set to n, no ID3 tagging is done
CDDBAVAIL=y
USEID3=y
USEID3V2=y

if [ -z "$OUTPUTDIR" ]; then
	OUTPUTDIR=$(pwd)
fi

if [ -z "$WAVOUTPUTDIR" ]; then
	WAVOUTPUTDIR="$OUTPUTDIR"
fi

# Load system defaults
if [ -r /etc/abcde.conf ]; then
	. /etc/abcde.conf
fi
# Load user preference defaults
if [ -r $HOME/.abcde.conf ]; then
	. $HOME/.abcde.conf
fi

# By this time, we need some HTTPGETOPTS already defined.
# If the user has defined a non-default HTTPGET method, we should not be empty.

if [ "$HTTPGETOPTS" = "" ] ; then
	case $HTTPGET in
		wget) HTTPGETOPTS="-q -O -";;
		curl) HTTPGETOPTS="-f -s";;
		fetch)HTTPGETOPTS="-q -o -";;
		ftp)  HTTPGETOPTS="-a -V -o - ";;
		*) log warning "HTTPGET in non-standard and HTTPGETOPTS are not defined." ;;
	esac
fi

# If the CDROM has not been set yet, find a suitable one.
# If this is a devfs system, default to /dev/cdroms/cdrom0
# instead of /dev/cdrom
if [ "$CDROM" = "" ] ; then
	if [ -e /dev/cdroms/cdrom0 ]; then
		CDROM=/dev/cdroms/cdrom0
	elif [ -e /dev/cdrom ]; then
		CDROM=/dev/cdrom
	elif [ -e /dev/cd0c ]; then
		CDROM=/dev/cd0c
	elif [ -e /dev/acd0c ]; then
		CDROM=/dev/acd0c
	elif [ -e /dev/disk1 ]; then
		CDROM=/dev/disk1
	fi
fi

# Parse command line options
#while getopts 1a:bc:C:d:Dehj:klLmMnNo:pPr:Rs:S:t:T:vVxw:W: opt ; do
while getopts 1a:bBc:C:d:Defghj:klLmMnNo:pPr:s:S:t:T:UvVxX:w:W:z opt ; do
	case "$opt" in
		1) ONETRACK=y ;;
		a) ACTIONS="$OPTARG" ;;
		A) EXPACTIONS="$OPTARG" ;;
		b) BATCHNORM=y ;;
		B) NOBATCHREPLAYGAIN=y ;;
		c) if [ -e "$OPTARG" ] ; then . "$OPTARG" ; else log error "config file \"$OPTARG\" cannot be found." ; exit 1 ; fi ;;
		C) DISCID="$( echo ${OPTARG#abcde.} | tr -d /)" ;;
		d) CDROM="$OPTARG" ;;
		D) set -x ;;
		h) usage; exit ;;
		e) ERASEENCODEDSTATUS=y ;;
		E) ENCODING="$OPTARG" ;;
		f) FORCE=y ;;
		g) NOGAP=y ;;
		i) INLINETAG=y ;;
		j) MAXPROCS="$OPTARG" ;;
		k) KEEPWAVS=y ;;
		l) LOWDISK=y ;;
		L) CDDBUSELOCAL=y ;;
		n) CDDBAVAIL=n ;;
		N) INTERACTIVE=n ;;
		m) DOSPLAYLIST=y ;;
		M) DOCUE=y ;;
		o) OUTPUTTYPE="$OPTARG" ;;
		p) PADTRACKS=y ;;
		P) USEPIPES=y ;;
		r) REMOTEHOSTS="$OPTARG" ;;
		R) CDDBLOCALRECURSIVE=y ;;
		s) SHOWCDDBFIELDS="$OPTARG" ;;
		S) CDSPEEDVALUE="$OPTARG" ;;
		t) STARTTRACKNUMBER="$OPTARG" ;;
		T) STARTTRACKNUMBER="$OPTARG" ; STARTTRACKNUMBERTAG="y" ;;
		U) CDDBPROTO=5 ;;
		v) 
		   echo "This is abcde v$VERSION."
		   echo "Usage: abcde [options] [tracks]"
		   echo "abcde -h for extra help"
		   exit
		   ;;
		V) EXTRAVERBOSE="y" ;;
		x) EJECTCD="y" ;;
		X) CUE2DISCID="$OPTARG" ;;
		w) COMMENT="$OPTARG" ;;
		W) if echo $OPTARG | grep "[[:digit:]]" > /dev/null 2>&1 ; then 
		     STARTTRACKNUMBER="${OPTARG}01" ; STARTTRACKNUMBERTAG="y" ; COMMENT="CD${OPTARG}"
		   else
		     log error "argument of -W must be integer"
			 exit 1
		   fi
		   ;;
		z) DEBUG=y ; CDROMREADERSYNTAX=debug ; EJECTCD="n" ;;
		?) usage; exit ;;
	esac
done

shift $(($OPTIND - 1))

# Here it comes the worse part of the whole thing. From here until we start
# ripping, we have a mixture of sanity checks, verifications, default settigs
# and other crazy stuff that interdepends, but hey, someone has to do it.

# If NOCDDBQUERY is set, don't query the CDDB server.
if [ "$NOCDDBQUERY" = "y" ]; then
	CDDBAVAIL="n"
fi

# If the user specified a flac file, then switch to special flac mode
if echo "$CDROM" | grep -i '.flac$' > /dev/null 2>&1 ; then
	if [ ! -f "$CDROM" ]; then
		log error "the defined file for FLAC ripping cannot be found" >&2
		exit 1
	fi
	vecho warning "switching to flac CDROMREADERSYNTAX..."
	CDROMREADERSYNTAX=flac
	# We have a builtin version of cue2discid...
	case "$CUE2DISCID" in
		builtin);;
		*) NEEDCUE2DISCID=y;;
	esac
	NEEDMETAFLAC=y
	EJECTCD=n
fi

# If the user provided a DISCID, disable eject
if [ -n "$DISCID" ] || [ "$CDROMREADERSYNTAX" = "flac" ]; then EJECTCD=n ; fi

# Check the available cd rippers in the system, from the ones we know.
if [ "$CDROMREADERSYNTAX" = "" ]; then
	for DEFAULT_CDROMREADER in $DEFAULT_CDROMREADERS; do
		if new_checkexec $DEFAULT_CDROMREADER; then
			CDROMREADERSYNTAX=$DEFAULT_CDROMREADER
			break
		fi
	done
	if [ "$CDROMREADERSYNTAX" = "" ]; then
		log error "no cdreader found in your PATH"
		log error "hints: are all dependencies installed? has the \$PATH been modified?"
		exit 1
	fi
fi

# Decide if we can continue.
if [ "$ONETRACK" = "y" ]; then 
	# FIXME # remove check as soon as we find out about the other readers
	case "$CDROMREADERSYNTAX" in
		flac) ;;
		cdparanoia) ;;
		cdda2wav) ;;
		*) log error "$CDROMREADERSYNTAX does not support ONETRACK mode"
		   exit 1 ;;
	esac
	if [ "$BATCHNORM" = "y" ]; then
		log warning "BATCHNORM mode is not compatible with ONETRACK mode. Disabling..."
		BATCHNORM=n
	fi
	if [ "$NOGAP" = "y" ]; then
		log warning "NOGAP mode is not compatible with ONETRACK mode. Disabling..."
		NOGAP=n
	fi
	# It does not matter how many tracks we want. In ONETRACK mode we grab them all
	# FIXME # allow ranges of tracks to be selected for onetrack ripping
	if [ $# -gt 0 ]; then
		log warning "ONETRACK mode selected, grabbing all tracks..."
	fi
else
	while [ $# -gt 0 ]; do
		# Range parsing code courtesy of Vincent Ho
		RSTART=$(echo $1 | cut -f1 -d-)
		REND=$(echo $1 | cut -f2 -d-)
		if [ "$RSTART" = "$REND" ]; then
			NEWTRACKS="$RSTART"
		else
			NEWTRACKS=$(f_seq_line $RSTART $REND)
		fi
		TRACKQUEUE=$(echo "$TRACKQUEUE" "$NEWTRACKS")
		shift
	done
fi

# List of valid actions: cddb,read,normalize,encode,tag,move,playlist,clean
# List of experimental actions: retag,transcode

# Determine what actions are to be done from $ACTIONS and set the
# following environment variables for them:
DOCDDB=n
DOREAD=n
DONORMALIZE=n
DOPREPROCESS=n
DOENCODE=n
DOPOSTPROCESS=n
DOTAG=n
DOMOVE=n
DOREPLAYGAIN=n
DOPLAYLIST=n
DOCLEAN=n
DOCUE=n

for ACTION in $(echo $ACTIONS | tr , \ )
do
	case $ACTION in
		default) DOCDDB=y; DOREAD=y; DOENCODE=y; DOTAG=y; DOMOVE=y; DOCLEAN=y;;
		cue) DOCUE=y ; MAKECUEFILE=y ;;
		cddb) DOCDDB=y;;
		read) DOREAD=y;;
		normalize) DONORMALIZE=y; DOREAD=y;;
#		preprocess) DOPREPROCESS=y; DOREAD=y;;
		encode) DOENCODE=y; DOREAD=y;;
#		postprocess) DOPREPROCESS=y; DOENCODE=y; DOREAD=y;;
		tag) DOTAG=y; DOREAD=y; DOENCODE=y; DOCDDB=y;;
		move) DOMOVE=y; DOTAG=y; DOREAD=y; DOENCODE=y; DOCDDB=y;;
		replaygain) DOCDDB=y; DOREAD=y; DOENCODE=y; DOTAG=y; DOMOVE=y; DOREPLAYGAIN=y;;
		playlist) DOCDDB=y; DOPLAYLIST=y;;
		clean) DOCLEAN=y;;
	esac
done

if [ "$DONORMALIZE" = "y" ] && [ "$DOREPLAYGAIN" = "y" ]; then
	# FIXME # should we abort on error or just inform the user?
	log warning "selected both normalize and replaygain actions"
fi

for SHOWCDDBFIELD in $(echo $SHOWCDDBFIELDS | tr , \ ); do
	case $SHOWCDDBFIELD in
		y*|Y*) SHOWCDDBYEAR="y";;
		g*|G*) SHOWCDDBGENRE="y";;
		*) ;;
	esac
done

# At this point a CDROM has to be defined, so we check it exists.
if [ X"$CDROM" != "X" ] ; then 
	if [ "$CDROMREADERSYNTAX" = "cdda2wav" ] && [ "$NEEDCDROMID" = "y" ] ; then
		if [ "$OSFLAVOUR" = "FBSD" ]; then
			if echo "$CDROMID" | grep "^[0-9],[0-9],[0-9]$" >/dev/null 2>&1 ; then :; else
				log error "CDROMID not in the right format for $CDROMREADERSYNTAX"
				log error "Use \"cdrecord -scanbus\" to obtain a adecuate ID an set CDROMID accordingly"
				exit 1
			fi
		fi
	elif [ ! -e "$CDROM" -a X"$DOREAD" = "Xy" ]; then
		log error "CDROM device cannot be found."
		exit 1
	fi
# avoid processing if we are not going to hit the CDROM.
elif [ X"$DOREAD" = "Xy" ]; then
	log error "CDROM has not been defined or cannot be found"
	exit 1
fi

# USEPIPES pre-tests, before we get into more serious stuff
# Not compatible with:
# - multiple outputs
# - normalize
# - lowdisk algorithm
# - anything else?
if [ X"$USEPIPES" = "Xy" ]; then
	if [ $(echo "$OUTPUTTYPE" | tr , \  | wc -w ) -gt 1 ]; then
		log error "Unix pipes not compatible with multiple outputs"
		exit 1
	fi
	if [ X"$DONORMALIZE" = "Xy" ]; then
		log error "Unix pipes not compatible with normalizer"
		# FIXME # Do we need to exit or shall we just disable the mode?
		exit 1
	fi
	if [ X"$BATCHNORM" = "Xy" ]; then
		log error "Unix pipes not compatible with BATCHNORM encoding"
		exit 1
	fi
	if [ X"$NOGAP" = "Xy" ]; then
		log error "Unix pipes not compatible with NOGAP encoding"
		exit 1
	fi
	if [ X"$DOENCODE" = "Xn" ]; then
		vecho warning "Disabling Unix pipes since we are not encoding!"
		USEPIPES=n
	fi
	if [ X"$LOWDISK" = "Xy" ]; then
		log error "Unix pipes not compatible with lowdisk algorithm"
		exit 1
	fi
fi

# LOWDISK pre-tests, before we get into more problematic stuff
# Not compatible with anything that needs all the files in the hard disc:
# - BATCHNORM
# - NOGAP lame mode
if [ X"$LOWDISK" = "Xy" ]; then
	if [ X"$BATCHNORM" = "Xy" ]; then
		log error "Unix pipes not compatible with BATCHNORM encoding"
		exit 1
	fi
	if [ X"$NOGAP" = "Xy" ]; then
		log error "Unix pipes not compatible with NOGAP encoding"
		exit 1
	fi
fi

# BATCHNORM pre-tests, before we get into serious problems
# Not compatible with 
if [ "$BATCHNORM" = "y" ] && [ "$DONORMALIZE" = "n" ]; then
	vecho warning "Disabling BATCHNORM since we are not normalizing!"
	BATCHNORM=n
fi

# Check the encoding format from the ones available in the system, if nothing has been configured in the system.
if [ X"$OUTPUTTYPE" = "X" ]; then
	for DEFAULT_OUTPUT in $( echo "$DEFAULT_OUTPUT_BINARIES" | tr , \ ); do
		DEFAULT_OUTPUT_FORMAT="$(echo $DEFAULT_OUTPUT | cut -d ":" -f 1)"
		DEFAULT_OUTPUT_BINARY="$(echo $DEFAULT_OUTPUT | cut -d ":" -f 2)"
		if [ -x $(which $DEFAULT_OUTPUT_BINARY) ] ; then
			OUTPUTTYPE=$DEFAULT_OUTPUT_FORMAT
			vecho "No default output type defined. Autoselecting $OUTPUTTYPE..." >&2
			break
		fi
	done
	if [ X"$OUTPUTTYPE" = "X" ]; then
		log error "no encoder found in the PATH"
		log error "hints: are all dependencies installed? has the \$PATH been modified?"
		exit 1
	fi
fi

# Decide which CDROM reader we're gonna use
case "$CDROMREADERSYNTAX" in
	cdparanoia|debug)
		CDROMREADER="$CDPARANOIA"
		CDROMREADEROPTS="$CDPARANOIAOPTS"
		;;
	cdda2wav)
		CDROMREADER="$CDDA2WAV"
		CDROMREADEROPTS="$CDDA2WAVOPTS"
		;;
	dagrab)
		CDROMREADER="$DAGRAB"
		CDROMREADEROPTS="$DAGRABOPTS"
		;;
	cddafs)
		CDROMREADER="$CDDAFS"
		CDROMREADEROPTS="$CDDAFSOPTS"
		;;
	flac)
		CDROMREADER="$FLAC"
		CDROMREADEROPTS="$FLACOPTS"
		;;
esac

# There's only one normalize...
case "$NORMALIZERSYNTAX" in
	default|normalize)
		NORMALIZER="$NORMALIZE"
		NORMALIZEROPTS="$NORMALIZEOPTS"
		;;
esac

# Allow -o OUTPUT(1):OPTIONS(1),...,OUTPUT(N):OPTIONS(N) mode of operation
if echo "$OUTPUTTYPE" | grep ":" > /dev/null 2>&1 ; then
	for OUTPUT in "$(echo "$OUTPUTTYPE" | tr \  \|| tr , \ | tr \| \ )"; do
		case "$OUTPUT" in
			vorbis:*|ogg:*)	OGGENCODEROPTSCLI="$( echo $OUTPUT | cut -d: -f2- )" ;;
			mp3:*)	MP3ENCODEROPTSCLI="$( echo $OUTPUT | cut -d: -f2- )" ;;
			flac:*)	FLACENCODEROPTSCLI="$( echo $OUTPUT | cut -d: -f2- )" ;;
			spx:*)	SPEEXENCODEROPTSCLI="$( echo $OUTPUT | cut -d: -f2- )" ;;
			mpc:*)	MPPENCODEROPTSCLI="$( echo $OUTPUT | cut -d: -f2- )" ;;
			m4a:*)  AACENCODEROPTSCLI="$( echo $OUTPUT | cut -d: -f2- )" ;;
		esac
	done
	for OUTPUT in "$(echo "$OUTPUTTYPE" | tr , \ )"; do
		TEMPOUTPUT=$( echo "$OUTPUT" | cut -d: -f1 )
		TEMPOUTPUTTYPE="${TEMPOUTPUTTYPE:+$TEMPOUTPUTTYPE,}$TEMPOUTPUT"
	done
	OUTPUTTYPE="$TEMPOUTPUTTYPE"
fi

# If nothing has been specified, use oggenc for oggs and lame for mp3s and flac
# for flacs and speexenc for speex and mppenc for mpps and faac for m4as

# Getting ready for multiple output changes
for OUTPUT in $(echo $OUTPUTTYPE | tr , \ )
do
	case $OUTPUT in
		vorbis|ogg)
			[ "$OGGENCODERSYNTAX" = "default" ] && OGGENCODERSYNTAX=oggenc
			[ "$DOTAG" = "y" ] && NEEDCOMMENTER=y
			[ "$DOREPLAYGAIN" = "y" ] && NEEDVORBISGAIN=y
			OGGOUTPUTCONTAINER=ogg
			;;
		mp3)
			[ "$MP3ENCODERSYNTAX" = "default" ] && MP3ENCODERSYNTAX=lame
			[ "$DOTAG" = "y" ] && NEEDTAGGER=y
			[ "$DOREPLAYGAIN" = "y" ] && NEEDMP3GAIN=y
			;;
		flac)
			[ "$FLACENCODERSYNTAX" = "default" ] && FLACENCODERSYNTAX=flac
			[ "$DOTAG" = "y" ] && NEEDMETAFLAC=y
			[ "$DOREPLAYGAIN" = "y" ] && NEEDMETAFLAC=y
			[ "$ONETRACK" = "y" ] && [ "$DOCUE" = "y" ] && NEEDMETAFLAC=y 
			;;
		spx)
			[ "$SPEEXENCODERSYNTAX" = "default" ] && SPEEXENCODERSYNTAX=speexenc
#			[ "$DOREPLAYGAIN" = "y" ] &&
			;;
		mpc)
			[ "$MPPENCODERSYNTAX" = "default" ] && MPPENCODERSYNTAX=mppenc
			[ "$DOREPLAYGAIN" = "y" ] && NEEDMPPGAIN=y
			;;
		m4a)
			[ "$AACENCODERSYNTAX" = "default" ] && AACENCODERSYNTAX=faac
			;;
		wav)
			if [ "$KEEPWAVS" = "y" ]; then
				vecho "Setting the KEEPWAVS option, since the resulting wav files were requested..."
			fi
			KEEPWAVS=move
			;;
		*)	log error "Invalid OUTPUTTYPE defined"
			exit 1
			;;
	esac
done

# decide which encoder
case "$MP3ENCODERSYNTAX" in
	lame)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$LAMEOPTS}"
		MP3ENCODER="$LAME"
		;;
	toolame)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$TOOLAMEOPTS}"
		MP3ENCODER="$TOOLAME"
		;;
	gogo)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$GOGOOPTS}"
		MP3ENCODER="$GOGO"
		;;
	bladeenc)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$BLADEENCOPTS}"
		MP3ENCODER="$BLADEENC"
		;;
	l3enc)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$L3ENCOPTS}"
		MP3ENCODER="$L3ENC"
		;;
	xingmp3enc)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$XINGMP3ENCOPTS}"
		MP3ENCODER="$XINGMP3ENC"
		;;
	mp3enc)
		MP3ENCODEROPTS="${MP3ENCODEROPTSCLI:-$MP3ENCOPTS}"
		MP3ENCODER="$MP3ENC"
		;;
esac
case "$OGGENCODERSYNTAX" in
	vorbize)
		OGGENCODEROPTS="${OGGENCODEROPTSCLI:-$VORBIZEOPTS}"
		OGGENCODER="$VORBIZE"
		;;
	oggenc)
		OGGENCODEROPTS="${OGGENCODEROPTSCLI:-$OGGENCOPTS}"
		OGGENCODER="$OGGENC"
		;;
esac
case "$FLACENCODERSYNTAX" in
	flac)
		FLACENCODEROPTS="${FLACENCODEROPTSCLI:-$FLACOPTS}"
		FLACENCODER="$FLAC"	
		# FLAC streams can be encapsulated on a Ogg transport layer
		if echo "$FLACENCODEROPTS" | egrep -- "(^| )--ogg($| )" > /dev/null 2>&1 ;then
			log error "FLAC on an Ogg container is not yet supported"
			log error "due to problem with adding comments to such files"
			exit 1
			FLACOUTPUTCONTAINER=ogg
		else
			FLACOUTPUTCONTAINER=flac
		fi
		;;
esac
case "$SPEEXENCODERSYNTAX" in
	speexenc)
		SPEEXENCODEROPTS="${SPEEXENCODEROPTSCLI:-$SPEEXENCOPTS}"
		SPEEXENCODER="$SPEEXENC"
		;;
esac
case "$MPPENCODERSYNTAX" in
	mppenc)
		MPPENCODEROPTS="${MPPENCODEROPTSCLI:-$MPPENCOPTS}"
		MPPENCODER="$MPPENC"
		;;
esac
case "$AACENCODERSYNTAX" in
	faac)
		AACENCODEROPTS="${AACENCODEROPTSCLI:-$AACENCOPTS}"
		AACENCODER="$AACENC"
		;;
esac

# and which tagger
if [ "$ID3TAGV" = "1" ]; then
	TAGGER="$ID3"
	TAGGEROPTS="$ID3OPTS"
else
	TAGGER="$ID3V2"
	TAGGEROPTS="$ID3V2OPTS"
fi

# Specific for NOGAP is the use of lame. Another encoder fails...
if [ "$NOGAP" = "y" ] && [ ! "$MP3ENCODER" = "lame" ]; then
	log warning "the NOGAP option is specific of lame. Deactivating..."
	NOGAP=n
fi

# Options for mkcue
case "$CUEREADERSYNTAX" in
	default|mkcue)
		CUEREADEROPTS="${CDROM}"
		CUEREADER="$MKCUE"
		;;
esac

# which information retrieval tool are we using?
case "$CDDBTOOL" in
	cddb) ;;
	musicbrainz) ;;
esac

# Check if both OGGEOUTPUTCONTAINER and FLACOUTPUTCONTAINER are the same, and differentiante them
if [ X"$OGGOUTPUTCONTAINER" = "Xogg" ] && [ X"$FLACOUTPUTCONTAINER" = "Xogg" ]; then
	log error "FLAC on an Ogg container is not yet supported"
	log error "due to problem with adding comments to such files"
	exit 1
	OGGOUTPUTCONTAINER=ogg.ogg
	FLACOUTPUTCONTAINER=flac.ogg
	vecho warning "modified file endings due to conflicting transport layers in Ogg/Vorbis and Ogg/FLAC"
fi

# Clean up nice options (either use '-n NICELEVEL or -NICELEVEL')

if [ "$ENCNICE" ]; then
	ENCNICE="-n $ENCNICE"
fi
if [ "$READNICE" ]; then
	READNICE="-n $READNICE"
fi
if [ "$DISTMP3NICE" ]; then
	DISTMP3NICE="-n $DISTMP3NICE"
fi

# Don't check for stuff if it's not needed
if [ "$REMOTEHOSTS" ]; then 
	NEEDDISTMP3=y
fi
if [ "$DONORMALIZE" = "y" ]; then
	NEEDNORMALIZER=y
fi
if [ "$EJECTCD" = "y" ]; then
	NEEDEJECT=y
fi
if [ ! "$CDDBAVAIL" = "n" ] && [ "$DOCDDB" = "y" ]; then
	if [ "$CDDBMETHOD" = "cddb" ]; then
		NEEDHTTPGET=y
	elif [ "$CDDBMETHOD" = "musicbrainz" ]; then
		:
	fi
fi
if [ "$DOCUE" = "y" ]; then
	NEEDCUEREADER=y
fi

if [ X"$CDSPEEDVALUE" != "X" ] && [ "$DOREAD" = "y" ]; then
	case "$CDROMREADERSYNTAX" in
		cdparanoia|debug) CDROMREADEROPTS="$CDPARANOIAOPTS -S $CDSPEEDVALUE" ;;
		### FIXME ### translate "cue2discid" from python to bash
		flac) NEEDMETAFLAC=y ; NEEDCUE2DISCID=y ; CDSPEEDVALUE="" ;;
		*) NEEDCDSPEED=y ;;
	esac
fi

###USEPIPESSUPPORT###

# Rippers with USEPIPE support
# FIXME # Include here all the rippers we can figure out support pipes
PIPERIPPER_cdparanoia="-"
PIPERIPPER_debug="-"
PIPERIPPER_flac="-c "

# Encoders with USEPIPE support
# FIXME # Include here all the encoders we can figure out support pipes
PIPE_lame="-"
PIPE_bladeenc="-"
PIPE_oggenc="-"
PIPE_flac="-"

# Figure out if we can use pipes with the ripper/encoder combination
# exit otherwise
if [ "$USEPIPES" = "y" ]; then
	PIPERIPPERSVARCHECK="PIPERIPPER_${CDROMREADERSYNTAX}"
	case "$OUTPUT" in
		mp3)
			PIPEENCODERSVARCHECK="PIPE_$MP3ENCODERSYNTAX" ;;
		vorbis|ogg)
			PIPEENCODERSVARCHECK="PIPE_$OGGENCODERSYNTAX" ;;
		flac)
			PIPEENCODERSVARCHECK="PIPE_$FLACENCODERSYNTAX" ;;
		spx)
			PIPEENCODERSVARCHECK="PIPE_$SPEEXENCODER" ;;
		mpc)
			PIPEENCODERSVARCHECK="PIPE_$MPPENCODER" ;;
	esac
	decho "PIPERIPPERSVARCHECK: $( eval echo "\$$PIPERIPPERSVARCHECK" )"
	if [ "$( eval echo "\$$PIPERIPPERSVARCHECK" )" = "$" ] || \
	   [ "$( eval echo "\$$PIPERIPPERSVARCHECK" )" = "" ] ; then
		log error "no support for pipes with given ripper"
		log error "read the USEPIPES file from the source tarball to get help."
		log error "On a Debian system, it is under /usr/share/doc/abcde/USEPIPES.gz"
		exit 1;
	fi
	decho "PIPEENCODERSVARCHECK: $( eval echo "\$$PIPEENCODERSVARCHECK" )"
	if [ "$( eval echo "\$$PIPEENCODERSVARCHECK" )" = "$" ] || \
	   [ "$( eval echo "\$$PIPEENCODERSVARCHECK" )" = "" ] ; then
		log error "no support for pipes with given encoder"
		log error "read the USEPIPES file from the source tarball to help"
		log error "on a Debian system, read /usr/share/doc/abcde/USEPIPES.gz"
		exit 1;
	fi
fi

# Make sure a buncha things exist
for X in $CDROMREADER $CDDISCID ${NEEDTAGGER+$TAGGER} $MP3ENCODER \
	$OGGENCODER $FLACENCODER $SPEEXENCODER $MPPENCODER \
	$AACENCODER \
	${NEEDHTTPGET+$HTTPGET} ${NEEDDISTMP3+$DISTMP3} \
	${NEEDCOMMENTER+$VORBISCOMMENT} ${NEEDMETAFLAC+$METAFLAC} \
	${NEEDNORMALIZER+$NORMALIZER} ${NEEDEJECT+$EJECT} \
	${NEEDDISKTOOL+disktool} ${NEEDCDSPEED+$CDSPEED} \
	${NEEDVORBISGAIN+$VORBISGAIN} ${NEEDMP3GAIN+$MP3GAIN} \
	${NEEDMPPGAIN+$MPPGAIN} ${NEEDCUEREADER+$CUEREADER} \
	${NEEDCUE2DISCID+$CUE2DISCID}
do
	checkexec "$X"
done

# And last but not least, check if we can diff between files. We do not abort,
# since diffing is not critical...
if [ -x $(which $DIFF) ]; then :; else
	vecho warning "Disabling diff since we cannot find it in the \$PATH..."
	DIFF=""
fi

## Now that we have metaflac, check if we need cue2discid
#case $CDROMREADERSYNTAX in
#	flac)
#		TRACKINFO=$($METAFLAC --show-tag=CDDB $CDROM | cut -d"=" -f2 | egrep "[a-f0-9]{8}")
#		if [ "$TRACKINFO" = "" ]; then 
#			checkexec ${NEEDCUE2DISCID+$CUE2DISCID}
#		fi
#		;;
#esac

CDROMREADER="$CDROMREADER $CDROMREADEROPTS"
CDDBTOOL="$CDDBTOOL $CDDBTOOLOPTS"
HTTPGET="$HTTPGET $HTTPGETOPTS"

# Here it used to say:
# One thousand lines in, we can start doing stuff with things
# Well, right now we are at line 3306 ;)

# Export needed things so they can be read in this subshell
export CDDBTOOL ABCDETEMPDIR TRACKQUEUE LOWDISK EJECTCD EJECT EJECTOPTS
export CDROM CDDBDATA REMOTEHOSTS MAXPROCS HTTPGET MD5SUM

if [ "$DOREAD" = "y" ]; then
	# User-definable function to set some things. Use it for
	#  - closing the CD tray with eject -t
	#  - set the CD speed value with eject -x
	vecho -n "Executing customizable pre-read function... "

	pre_read # Execute the user-defined pre-read funtion. Close the CD with it.

	vecho "done."
fi

case "$CDDBMETHOD" in
	cddb)
		do_discid # Get ABCDETEMPDIR created and status file initialized
		;;
	musicbrainz)
		do_musicbrainz id
		;;
esac

if [ "$DOCDDB" = "y" ]; then
	# start with a sane default:
	CDDBLOCALSTATUS=notfound
	if [ $CDDBUSELOCAL = "y" ]; then
		do_localcddb
	fi
	if checkstatus cddb-choice > /dev/null; then
		:
	else 
		if [ "$CDDBLOCALSTATUS" = "notfound" ] ; then
			case "$CDDBMETHOD" in
				cddb)
					do_cddbstat
					do_cddbquery
					do_cddbread
					;;
				musicbrainz)
					do_musicbrainz
					;;
			esac
		fi
	fi
	do_cddbedit

	eval "$($CDDBTOOL parse "$CDDBDATA")"
fi

# Before reading tracks, we set the speed of the device

if [ X"$CDSPEEDVALUE" != "X" ]; then
	case "$CDROMREADERSYNTAX" in
		cdparanoia|debug) ;;
		flac) ;;
		*) do_cdspeed ;;
	esac
fi

# Define the first and last track, since we might need them later in several places
FIRSTTRACK=$( get_first $TRACKQUEUE )
LASTTRACK=$( get_last $TRACKQUEUE )

if [ -f "$ABCDETEMPDIR/status" ] && [ X"$ERASEENCODEDSTATUS" = "Xy" ]; then
	mv "$ABCDETEMPDIR/status" "$ABCDETEMPDIR/status.old"
	grep -v ^encodetracklocation- < "$ABCDETEMPDIR/status.old" \
		| grep -v ^encode-output > "$ABCDETEMPDIR/status"
fi

if checkstatus onetrack ; then ONETRACK=y ; fi

if [ "$ONETRACK" = "y" ]; then 
	# Reuse the CUEFILE in case we created it (now or in a previous run)
	if CUEFILE=$(checkstatus cuefile); then
		IMPORTCUESHEET=y
	fi
fi

# Create playlist if needed (backgroundable) and start reading in tracks

(

if [ ! "$ONETRACK" = "y" ]; then
	if [ "$DOPLAYLIST" = "y" ]; then
		echo Creating playlist... >&2
		do_playlist
	fi
fi

# For the lowdisk option, only one program is running at once so the encoder
# can be unsilenced right away.
if [ "$LOWDISK" = "y" ] || [ "$ONETRACK" = "y" ]; then
	echo "encode-output=loud" >> "$ABCDETEMPDIR/status"
fi

if [ "$ONETRACK" = "y" ]; then 
	TRACKS="$FIRSTTRACK"
	if [ "$USEPIPES" = "y" ]; then
		if checkstatus readencodetrack-$FIRSTTRACK; then :; else
			do_cdread onetrack $FIRSTTRACK $LASTTRACK | do_encode $FIRSTTRACK %local0% > /dev/null 2>&1
		fi
	else
		if checkstatus readtrack-$FIRSTTRACK; then :; else
			do_cdread onetrack $FIRSTTRACK $LASTTRACK
		fi
	fi
else
	for UTRACKNUM in $TRACKQUEUE
	do
		if [ "$DOREAD" = "y" ]; then
			if [ "$USEPIPES" = "y" ]; then
				if checkstatus readencodetrack-$UTRACKNUM; then :; else
					# Read, pipe, shut up!
					do_cdread $UTRACKNUM | do_encode $UTRACKNUM %local0% > /dev/null 2>&1
				fi
			else
				if checkstatus readtrack-$UTRACKNUM; then :; else
					do_cdread $UTRACKNUM
				fi
				if [ "$?" != "0" ]; then
					# CD read failed - don't give the goahead to
					# the encoder
					echo NO
					exit
				fi
			fi
		fi
		if [ "$NOGAP" = "y" ] || [ "$BATCHNORM" = "y" ]; then
		    :
		else
			# If we are not reading, set the encode output to loud already, so
			# that we can see the output of the first track.
			if [ "$MAXPROCS" = "1" ] && [ ! "$DOREAD" = "y" ]; then
				echo "encode-output=loud" >> "$ABCDETEMPDIR/status"
			fi
			echo NEXTTRACK # Get the encoder machine churning again
			if [ "$DOREAD" = "y" ]; then
				if [ "$LOWDISK" = "y" ] && [ "$DOENCODE" = "y" ]; then
					until checkstatus encodetrack-$UTRACKNUM
					do
						if checkerrors encodetrack-$UTRACKNUM; then
							break
						fi
						sleep 2
					done
				fi
			fi
		fi
	done
fi

# Now that we're done the encoding can be loud again -
# if we're not using SMP.
if [ "$MAXPROCS" = "1" ]; then
	echo "encode-output=loud" >> "$ABCDETEMPDIR/status"
fi

# All tracks read, start encoding.
if [ "$NOGAP" = "y" ] || [ "$BATCHNORM" = "y" ] || [ "$ONETRACK" = "y" ]; then
	echo NEXTTRACK
fi

# Execute the user-defined post_read funtion before ejecting CD
post_read

# We are now finished with the cdrom - it can be safely ejected. Note that
# abcde will not have completed yet.
if [ "$EJECTCD" = "y" ] && [ -x $(which $EJECT) ]; then
	# We check if the disk we are processing is actually the disk inside the 
	# CD tray. If not, we do not eject the CD, since it might be so that the
	# user ejected it manually.
	#CURRENTTRACKINFO=$($CDDISCID $CDROM)
	#if if [ "$?" != "1" ] && [ "$CURRENTTRACKINFO" = "$TRACKINFO" ] ; then 
	# More FreeBSD bits.
	if [ X"$(uname)" = X"FreeBSD" ] ; then
		# FreeBSD eject uses the EJECT environment variable to name the CDROM
		# but in this script EJECT is in the envionment and names the program
		eject=$EJECT
		unset EJECT
		# The FreeBSD eject needs "adc0" not "/dev/adc0c"
		cd="$(echo $CDROM | sed -e 's=.*/==;s=[a-h]$==;')"
		$eject $EJECTOPTS $cd
	elif [ X"$(uname)" = X"Darwin" ] ; then
		disktool -e ${CDROM#/dev/} 0
	else
		$EJECT $EJECTOPTS "$CDROM"
	fi
	#fi
fi

) | (

## Do we need to pre-process 
#if [ x"$PREPROCESS" = "x" ] ; then
#	cat
#else
#	for PRETRACKNUM in $TRACKQUEUE
#	do
#		read GOAHEAD
#		if [ "$GOAHEAD" = "NO" ]; then break; fi
#		PREPROCEED=
#		until [ $PREPROCEED ]
#		do
#			if checkstatus readtrack-$PRETRACKNUM; then PREPROCEED=y; break; fi
#			# all locations are working, wait and try again later
#			if [ ! $PREPROCEED ]; then sleep 3; fi
#		done
#		( do_preprocess $PRETRACKNUM 
#		echo "NEXTTRACK"
#		) &
#	done
#fi
#
#) | (

# In BATCHNORM and/or NOGAP modes, we want all tracks to be read first.
#BACK
if [ "$BATCHNORM" = "y" ] || [ "$NOGAP" = "y" ]; then
	read GOAHEAD # For blocking - will contain either "NO" or "NEXTTRACK"
	if [ "$GOAHEAD" = "NO" ]; then break; fi
	for LASTTRACK in $TRACKQUEUE; do :; done
	if checkstatus readtrack-$LASTTRACK; then
		if [ "$DONORMALIZE" = "y" ] && [ "$BATCHNORM" = "y" ]; then
			if checkstatus normalizetrack-$LASTTRACK; then :; else do_batch_normalize; fi
			if checkerrors batch-normalize; then exit 1; fi
		fi
		if [ "$DOENCODE" = "y" ] && [ "$NOGAP" = "y" ]; then
			if [ "$DONORMALIZE" = "y" ]; then
				for UTRACKNUM in $TRACKQUEUE
				do
					if checkstatus readtrack-$UTRACKNUM; then
						if checkstatus normalizetrack-$UTRACKNUM; then :; else do_normalize $UTRACKNUM; fi
					fi
				done
			fi
			if checkstatus encodetrack-$LASTTRACK; then :; else do_nogap_encode; fi
			if checkerrors nogap-encode; then exit 1; fi
		fi
	fi
fi

# If we are using ONETRACK, we can proceed with the normal encoding using just the $FIRSTTRACK as TRACKQUEUE
if [ "$ONETRACK" = "y" ] ; then
	TRACKQUEUE="$FIRSTTRACK"
	TRACKS="$FIRSTTRACK"
fi

# Do the encoding, including parallelization of remote encoding
# Figure out where each track is going to be encoded
ENCODELOCATIONS="$(echo $REMOTEHOSTS | tr , ' ')"
if [ "$MAXPROCS" != "0" ]; then
	for NUM in $(f_seq_row 1 "$MAXPROCS")
	do
		ENCODELOCATIONS="$ENCODELOCATIONS %local$NUM%"
	done
fi
# Strip whitespace
ENCODELOCATIONS=$(echo $ENCODELOCATIONS)
for UTRACKNUM in $TRACKQUEUE
do
	# Wait for our cue
	read GOAHEAD # For blocking - will contain either "NO" or "NEXTTRACK"
	if [ "$GOAHEAD" = "NO" ]; then break; fi
	# find out where this track is to be encoded
	if [ "$DOENCODE" = "y" -a "$USEPIPES" != "y" ]; then
		# Make sure we have a place to encode this, if not, exit stage right
		if [ -z "$ENCODELOCATIONS" ]; then
			continue
		fi
		PROCEED=
		until [ $PROCEED ]
		do
			for LOCATION in $ENCODELOCATIONS
			do
				PREVIOUSTRACK="$(checkstatus encodetracklocation-$LOCATION)"
				# check first if a track has ever been assigned to this location
				if [ -z "$PREVIOUSTRACK" ]; then PROCEED=y; break; fi
				# If it errored out, rebuild $ENCODELOCATIONS without this location in it
				if checkerrors encodetrack-$PREVIOUSTRACK; then
					for TEMPLOCATION in $ENCODELOCATIONS
					do
						if [ "$TEMPLOCATION" != "$LOCATION" ]; then
							TEMPENCODELOCATIONS="$TEMPENCODELOCATIONS $TEMPLOCATION"
						fi
					done
					ENCODELOCATIONS=$(echo $TEMPENCODELOCATIONS)
					ABORT=y
					PROCEED=y
					break
				fi
				# We're still here, this location must have been previously assigned,
				# and last completed without error - check if it's done with the
				# previous track yet
				if checkstatus encodetrack-$PREVIOUSTRACK; then PROCEED=y; break; fi
			done
			# all locations are working, wait and try again later
			if [ ! $PROCEED ]; then sleep 3; fi
		done
		# Record the location we're about to encode the next track at
		echo "encodetracklocation-$LOCATION=$UTRACKNUM" >> "$ABCDETEMPDIR/status"
	fi
	# Don't proceed with the rest of the loop if we can't encode
	if [ "$ABORT" ]; then continue; fi
	## FIXME ## Add here 
	## run_command tagtrack-$OUTPUT-$1 $METAFLAC $METAFLACOPTS ${IMPORTCUESHEET:+--import-cuesheet-from="$ABCDETEMPDIR/$CUEFILE"} --import-tags-from=- "$ABCDETEMPDIR/track$1.$FLACOUTPUTCONTAINER"
	# Set TRACKNUM, TRACKNAME
	if [ -e "$CDDBDATA" ]; then
		if [ "$ONETRACK" = "y" ]; then 
			TRACKNAME="$DALBUM"
			TRACKNUM="$FIRSTTRACK"
			splitvarious
		else
			TRACKNUM=$UTRACKNUM
			CDDBTRACKNUM=$(expr $UTRACKNUM - 1)
			getcddbinfo TRACKNAME
			splitvarious
		fi
	fi
	# You can't encode a file which needs to be normalized before finishing
	# You can't tag a file before it's finished encoding -
	# thus all of this is backgrounded together
	(
	if [ "$DONORMALIZE" = "y" ]; then
		if checkstatus readtrack-$UTRACKNUM; then
			if checkstatus normalizetrack-$UTRACKNUM; then :; else do_normalize $UTRACKNUM; fi
		fi
	fi
	if [ "$DOENCODE" = "y" -a "$USEPIPES" != "y" ]; then
		if checkstatus readtrack-$UTRACKNUM; then
			#if checkstatus encodetrack-$UTRACKNUM; then :; else do_encode $UTRACKNUM $LOCATION; fi
			if [ "$DONORMALIZE" = "y" ]; then
				if checkstatus normalizetrack-$UTRACKNUM; then
					if checkstatus encodetrack-$UTRACKNUM; then :; else do_encode $UTRACKNUM $LOCATION $OUTPUT; fi
				fi
			else
				if checkstatus encodetrack-$UTRACKNUM; then :; else do_encode $UTRACKNUM $LOCATION $OUTPUT; fi
			fi
		fi
	fi
	if [ "$DOTAG" = "y" ]; then
		if checkstatus encodetrack-$UTRACKNUM; then
			if checkstatus tagtrack-$UTRACKNUM; then :; else do_tag $UTRACKNUM; fi
		fi
		# Lets tag the cue file
		if checkstatus cleancuefile >/dev/null; then :; else
			if checkstatus cuefile >/dev/null ; then 
				do_cleancue
			fi
		fi
	fi
	if [ "$DOMOVE" = "y" ]; then
		if checkstatus tagtrack-$UTRACKNUM; then
			if checkstatus movetrack-$UTRACKNUM; then :; else do_move $UTRACKNUM; fi
		fi
	fi
	) &
done


# Go through it again and make sure there's no distmp3 stragglers, otherwise
# we'll delete the files they're working on
# Do NOT play ball if there is no ball to play (so ABORT if needed)
## FIXME ## Check also for files which are encoded using PIPEs.
if [ "$DOENCODE" = "y" ] && [ "$USEPIPES" != "y" ] && [ ! "$ABORT" ]; then
	PROCEED=
	until [ $PROCEED ]
	do
		PROCEED=y
		for LOCATION in $ENCODELOCATIONS
		do
			CHECKTRACK="$(checkstatus encodetracklocation-$LOCATION)"
			# "How can he give us a status update, if he's DEAD?"
			if checkstatus encodetrack-$CHECKTRACK; then
				continue
			fi
			# Nothing to see here please go quietly back to your homes
			if [ -z "$CHECKTRACK" ]; then continue; fi
			# You're still here? Maybe there is something...
			if checkstatus encodetrack-$CHECKTRACK; then :;	else PROCEED= ; break; fi
		done
		# hold up
		if [ ! $PROCEED ]; then sleep 5; fi
	done
fi
# If the above didn't catch the stragglers, this will
wait
if [ "$DOREPLAYGAIN" = "y" ]; then
	do_replaygain
fi

# Check to see if run_command logged any errors
if [ -f "$ABCDETEMPDIR/errors" ]; then
	log error "The following commands failed to run:"
	cat "$ABCDETEMPDIR/errors"
	# Don't clean up
	DOCLEAN=n
fi
if [ "$KEEPWAVS" = "y" ];then
	# Don't clean up
	DOCLEAN=n
fi
# Check if we have moved all the formats we had previously encoded, if we are not using the FORCE.
if [ "$DOCLEAN" = "y" ] && [ ! "$FORCE" = "y" ]; then
	ENCODED_FORMATS=$(egrep "^encodetrack-(.{3,6})-(.{1,2})$" "$ABCDETEMPDIR/status" | cut -d"-" -f2 | sort -u | tr '\n' '|')
	MOVED_FORMATS=$(egrep "^movetrack-output-(.{3,6})$" "$ABCDETEMPDIR/status" | cut -d"-" -f3 | sort -u | tr '\n' '|')
	if [ "$ENCODED_FORMATS" != "$MOVED_FORMATS" ]; then
		log warning "The encoded formats does not match with the moved ones"
		log warning "Formats encoded: $( echo $ENCODED_FORMATS | tr "|" " " )"
		log warning "Formats moved: $( echo $MOVED_FORMATS | tr "|" " " )"
		log warning "Use \"abcde -a clean -f -C $DISCID\" to force the removal of the remaining data."
		DOCLEAN=n
	fi
fi
if [ "$DOCLEAN" = "y" ]; then
	# Wipe all the evidence
	# Gimme gimme gimme some more time!
	sleep 5
	rm -rf "$ABCDETEMPDIR"
	echo "Finished."
else
	echo "Finished. Not cleaning $ABCDETEMPDIR."
fi
)
exit 0

# b:is_bash
# vim:tabstop=4
