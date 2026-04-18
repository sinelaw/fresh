#!/bin/sh

# BSD 2-Clause License
#
# Copyright (c) 2024-2026 Shi Yanling
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

EDITOR=${EDITOR:-vi}
export LC_ALL=C
unset LESS
cwd=$(pwd)
uid=$(id -u)

sffpipe=$1
sffdir=${sffpipe%/*}
tmpdir=${TMPDIR:-/tmp}
[ ! -w "$tmpdir" ] && tmpdir=$sffdir

exbuf1="${sffdir}/.exec-buf1"
exbuf2="${sffdir}/.exec-buf2"
lastop="${sffdir}/.last-operation"
cpbuf="${sffdir}/.copy-buf"
tsel="${tmpdir}/sff-tmpsel-$uid"
tbuf1="${tmpdir}/sff-tmpbuf1-$uid"
tbuf2="${tmpdir}/sff-tmpbuf2-$uid"


# === sff extension functions ===

sffpipe_clear_sel()
{
	[ -p "$sffpipe" ] && printf "." >"$sffpipe"
}

sffpipe_refresh()
{
	[ "$1" = '-c' ] && _x='.' || _x=''
	[ -p "$sffpipe" ] && printf "*%s" "$_x" >"$sffpipe"
}

sffpipe_sel_file()
{
	[ -p "$sffpipe" ] && printf "@%s\0" "$1" >"$sffpipe"
}

sffpipe_enter_dir()
{
	[ -p "$sffpipe" ] && printf ">%s\0" "$1" >"$sffpipe"
}

sffpipe_get_sel()
{
	[ -p "$sffpipe" ] && printf "%s" "$$" >"$sffpipe"
	sel=$sffpipe
}

sff_init_bufs()
{
	[ -e "$exbuf1" ] && [ -e "$exbuf2" ] && [ -e "$lastop" ] && [ -e "$cpbuf" ] && return 0
	touch -a "$exbuf1" "$exbuf2" "$lastop" "$cpbuf"
	chmod 600 "$exbuf1" "$exbuf2" "$lastop" "$cpbuf"
	[ "$uid" -eq 0 ] && ls -nd "$sffpipe" | { read -r _ _ _x _; chown "$_x" "$exbuf1" "$exbuf2" "$lastop" "$cpbuf"; }
}

sff_abort()
{
	rm -f "$tsel" "$tbuf1" "$tbuf2"
	[ "$1" ] && rm -f "$1"
	exit 0
}

sff_pwd_perm()
{
	if [ ! -w "$cwd" ]; then
		printf "\n%s: Permission denied\n" "$cwd"
		printf "Press Enter to continue "; read -r _x
		exit 0
	fi
}

sff_new()
{
	sff_pwd_perm
	sff_init_bufs
	_tlist="${tmpdir}/sff-create-files-$uid"
	: >"$_tlist"

	while true; do
		LC_ALL= $EDITOR "$_tlist"
		[ -e "$_tlist" ] && sed -e 's|^/*||' -e 's|[ \t]*$||' -e '/^$/d' "$_tlist" >"$tbuf1"
		[ ! -s "$tbuf1" ] && sff_abort "$_tlist"

		sed 's|/.*$||' "$tbuf1" | sort -u >"$tbuf2"
		_existf=$(tr '\n' '\0' <"$tbuf2" | xargs -0 ls -1d -- 2>/dev/null | head -n 80)
		[ -z "$_existf" ] && break

		printf "\n%s\n" "$_existf"
		echo "^^^ file exists"
		printf "(e)dit list / (c)ancel [e]: "; read -r _x
		case "$_x" in [cC]) sff_abort "$_tlist";; esac
	done

	_pwd=$(printf '%s' "$cwd" | tr '\n' '\035' | sed 's/[\|&]/\\&/g')
	sed "s|^|$_pwd/|" "$tbuf2" | tr '\n\035' '\0\n' >"$exbuf2"
	sed -e '/\//!d' -e 's|/[^/]*$||' -e "s|^|$_pwd/|" "$tbuf1" | sort -u | tr '\n' '\0' >"$exbuf1"
	printf "\n" >>"$exbuf1"
	sed -e '/\/$/d' -e "s|^|$_pwd/|" "$tbuf1" | tr '\n' '\0' >>"$exbuf1"

	rm -f "$_tlist" "$tbuf1" "$tbuf2"
	sff_do_new
}

sff_do_new()
{
	printf "new" >"$lastop"
	sffpipe_sel_file "$(tr '\n\0' '\035\n' <"$exbuf2" | head -n 1 | tr -d '\n' | tr '\035' '\n')"

	_err=''
	[ "$(head -c 1 "$exbuf1" | tr -d '\n')" ] \
	&& { head -n 1 "$exbuf1" | tr -d '\n' | tr '\035' '\n' | LC_ALL= xargs -0 mkdir -p || _err=1; }

	[ "$(tail -c 1 "$exbuf1" | tr -d '\n' | tr '\0' 'A')" ] \
	&& { tail -n +2 "$exbuf1" | tr '\035' '\n' | LC_ALL= xargs -0 touch || _err=1; }
	[ "$_err" ] && { printf "Press Enter to continue "; read -r _x; }
}

sff_undo_new()
{
	[ ! -s "$exbuf2" ] && exit 0
	_op=$(cat "$lastop")

	echo ""
	tr '\n\0' '\035\n' <"$exbuf2" | head -n 160
	echo "^^^" $(tr '\n\0' '\035\n' <"$exbuf2" | wc -l) "files will be deleted"
	printf "Undo '%s'? (y/n) [n]: " "$_op"; read -r _x
	case "$_x" in [yY]) :;; *) exit 0;; esac

	printf "un%s" "$_op" >"$lastop"
	sffpipe_refresh

	LC_ALL= xargs -0 rm -rf <"$exbuf2" \
	|| { printf "Press Enter to continue "; read -r _x; }
}

sff_write_cbuf()
{
	sff_init_bufs
	sffpipe_get_sel
	tr '\n\0' '\035\n' <"$sel" >"$cpbuf"
	if [ "$1" = 'mv' ]; then
		chmod u+s "$cpbuf"
	else
		chmod u-s "$cpbuf"
	fi
	sffpipe_clear_sel
}

sff_view_cbuf()
{
	echo ""
	if [ -s "$cpbuf" ]; then
		head -n 160 "$cpbuf"
		echo $(wc -l <"$cpbuf") "file(s) in buffer"
	else
		echo "Buffer is empty"
	fi
	printf "Press Enter to continue "; read -r _x
}

sff_clear_cbuf()
{
	[ -s "$cpbuf" ] && : >"$cpbuf"
}

sff_paste()
{
	[ ! -s "$cpbuf" ] || [ "$(find "$cpbuf" -mmin +30)" ] && exit 0
	sff_pwd_perm

	_x=''; _op='copy'
	[ -u "$cpbuf" ] && _op='move'

	_existf=$(sed 's|^.*/||' "$cpbuf" | tr '\n\035' '\0\n' | xargs -0 ls -1d -- 2>/dev/null | head -n 80)
	if [ "$_existf" ]; then
		printf "\n%s\n" "$_existf"
		echo "^^^ files exist"
		printf "(s)kip all / (i)nteractive / (o)verwrite all / (c)ancel [c]: "; read -r _x
		case "$_x" in [sSiIoO]) :;; *) sff_abort;; esac
	fi

	_pwd=$(printf "%s" "$cwd" | tr '\n' '\035' | sed 's/[\|&]/\\&/g')
	sed "s|$|$_pwd/|" "$cpbuf" | tr '\037\n\035' '\0\0\n' >"$exbuf1"

	case "$_op" in
	'copy') sed "s|^.*/|$_pwd/|" "$cpbuf" | tr '\n\035' '\0\n' >"$exbuf2"
		[ "$1" ] && : >"$cpbuf";;
	'move') sed "s|^.*/|$_pwd/|" "$cpbuf" | paste -d '' - "$cpbuf" | tr '\037\n\035' '\0\0\n' >"$exbuf2"
		: >"$cpbuf";;
	esac

	_x=${_x:-'w'}
	sff_do_paste "$_op" "$_x"
}

sff_do_paste()
{
	printf "%s" "$1" >"$lastop"
	[ "$2" != 'w' ] && touch -mt 202310011200.00 "$lastop"
	sffpipe_sel_file "$(tr '\n\0' '\035\n' <"$exbuf2" | head -n 1 | tr -d '\n' | tr '\035' '\n')"

	unset LC_ALL
	case "$1" in
	'copy') printf "\nCopying...\n"
		case "$2" in
		[oOw]) xargs -0 -n 2 cp -afv <"$exbuf1";;
		[sS]) xargs -0 -n 2 cp -anv <"$exbuf1";;
		[iI]) xargs -0 -n 2 -o cp -aiv <"$exbuf1";;
		esac
		;;
	'move') printf "\nMoving...\n"
		case "$2" in
		[oOw]) xargs -0 -n 2 mv -fv <"$exbuf1";;
		[sS]) xargs -0 -n 2 mv -nv <"$exbuf1";;
		[iI]) xargs -0 -n 2 -o mv -iv <"$exbuf1";;
		esac
		;;
	esac || { printf "Press Enter to continue "; read -r _x; }
}

sff_rename()
{
	sff_init_bufs
	sffpipe_get_sel
	tr '\n\0' '\035\n' <"$sel" >"$tsel"
	[ ! -s "$tsel" ] && exit 0

	sed 's|/[^/]*$||' "$tsel" >"$tbuf1"
	_tlist="${tmpdir}/sff-rename-$uid"
	if [ "$(sort -u "$tbuf1" | wc -l)" -eq 1 ]; then
		sed 's|^.*/||' "$tsel" >"$_tlist"
	else
		cat "$tsel" >"$_tlist"
	fi

	while true; do
		LC_ALL= $EDITOR "$_tlist"
		sed -e 's|^.*/||' -e 's|^[ \t]*$||' "$_tlist" | paste -d '/' "$tsel" "$tbuf1" - \
		| sed -e '/^\(.*\)\1$/d' -e '/^/d' -e '/\/$/d' >"$tbuf2"
		[ ! -s "$tbuf2" ] && sff_abort "$_tlist"

		_dupnames=$(cut -d '' -f 2 "$tbuf2" | sort | uniq -d | head -n 80)
		[ "$_dupnames" ] && printf "\n%s\n^^^ duplicate names\n" "$_dupnames"

		_existf=$(cut -d '' -f 2 "$tbuf2" | tr '\n\035' '\0\n' | xargs -0 ls -1d -- 2>/dev/null | head -n 80)
		[ "$_existf" ] && printf "\n%s\n^^^ file exists\n" "$_existf"

		[ -z "$_dupnames" ] && [ -z "$_existf" ] && break
		printf "(e)dit list / (c)ancel [e]: "; read -r _x
		case "$_x" in [cC]) sff_abort "$_tlist";; esac
	done

	tr '\037\n\035' '\0\0\n' <"$tbuf2" >"$exbuf1"
	sed 's/\(.*\)\(.*\)/\2\1/' "$tbuf2" | tr '\037\n\035' '\0\0\n' >"$exbuf2"

	rm -f "$tsel" "$_tlist" "$tbuf1" "$tbuf2"
	sff_do_rename
}

sff_do_rename()
{
	printf "rename" >"$lastop"
	sffpipe_sel_file "$(tr '\n\0' '\035\n' <"$exbuf2" | head -n 1 | tr -d '\n' | tr '\035' '\n')"

	LC_ALL= xargs -0 -n 2 mv -nv <"$exbuf1" \
	|| { printf "Press Enter to continue "; read -r _x; }
}

sff_undo_move()
{
	[ ! -s "$exbuf2" ] && exit 0
	_op=$(cat "$lastop")
	echo ""
	xargs -0 -n 2 printf "%s -> %s\n" <"$exbuf2" | head -n 160
	printf "Undo '%s'? (y/n) [n]: " "$_op"; read -r _x
	case "$_x" in [yY]) :;; *) exit 0;; esac

	printf "un%s" "$_op" >"$lastop"
	sffpipe_sel_file "$(tr '\n\0' '\035\n' <"$exbuf1" | head -n 1 | tr -d '\n' | tr '\035' '\n')"

	LC_ALL= xargs -0 -n 2 mv -n <"$exbuf2" \
	|| { printf "Press Enter to continue "; read -r _x; }
}

sff_duplicate()
{
	sff_init_bufs
	sffpipe_get_sel
	tr '\n\0' '\035\n' <"$sel" >"$tsel"
	[ ! -s "$tsel" ] && exit 0

	printf "\nNumber of copies / (c)ancel [1]: "; read -r _x
	_x=$(printf "%s" "${_x:-'1'}" | tr -cd '0-9')
	_x=${_x#"${_x%%[!0]*}"}
	[ -z "$_x" ] && exit 0

	: >"$exbuf1"; : >"$exbuf2"
	while IFS= read -r _path; do
		_num=1
		_path=$(printf "%s" "$_path" | tr '\035' '\n')

		for _ in $(seq "$_x"); do
			_npath="${_path}_$_num"
			_i=$_num
			while [ -e "$_npath" ]; do
				_i=$((_i + 1))
				_npath="${_path}_$_i"
			done
			_num=$((_i + 1))

			printf "%s\0%s\0" "$_path" "$_npath" >>"$exbuf1"
			printf "%s\0" "$_npath" >>"$exbuf2"
		done
	done <"$tsel"
	rm -f "$tsel"

	[ ! -s "$exbuf1" ] && sff_abort
	sff_do_duplicate
}

sff_do_duplicate()
{
	printf "duplicate" >"$lastop"
	sffpipe_sel_file "$(tr '\n\0' '\035\n' <"$exbuf2" | head -n 1 | tr -d '\n' | tr '\035' '\n')"

	echo "Duplicating..."
	LC_ALL= xargs -0 -n 2 cp -an <"$exbuf1" \
	|| { printf "Press Enter to continue "; read -r _x; }
}

sff_delete()
{
	sffpipe_get_sel
	tr '\n\0' '\035\n' <"$sel" >"$tsel"
	[ ! -s "$tsel" ] && sff_abort

	echo ""
	head -n 160 "$tsel"
	printf "Permanently delete %s files? (y/n) [n]: " $(wc -l <"$tsel"); read -r _x
	case "$_x" in [yY]) :;; *) exit 0;; esac
	sffpipe_refresh -c

	tr '\n\035' '\0\n' <"$tsel" | LC_ALL= xargs -0 rm -rf \
	|| { printf "Press Enter to continue "; read -r _x; }
	rm -f "$tsel"
}

sff_edit_file()
{
	sffpipe_get_sel
	tr '\n\0' '\035\n' <"$sel" >"$tsel"
	[ ! -s "$tsel" ] && sff_abort

	while IFS= read -r _path; do
		_path=$(printf "%s" "$_path" | tr '\035' '\n')
		set -- "$@" "$_path"
	done <"$tsel"

	rm -f "$tsel"
	sffpipe_refresh
	LC_ALL= exec $EDITOR "$@"
}

sff_chmod_chown()
{
	printf "\nMode or User:Group (e.g., 644, a+x, u:g): "; read -r _x
	[ -z "$_x" ] && exit 0
	printf "Apply recursively? (y/n) [n]: "; read -r _x2
	case "$_x2" in [yY]) _x2='-R';; *) _x2='';; esac

	sffpipe_get_sel
	case "$_x" in
	*:*) LC_ALL= xargs -0 chown $_x2 $_x <"$sel";;
	*) LC_ALL= xargs -0 chmod $_x2 $_x <"$sel";;
	esac || { printf "Press Enter to continue "; read -r _x; }
	sffpipe_refresh
}

sff_find()
{
	printf "\nSearch pattern (with wildcards *, ?): "; read -r _x
	[ -z "$_x" ] && exit 0
	printf "More options (optional): "; read -r _x2
	echo "Searching... (in $cwd)"
	{ printf "?"; find ./ $_x2 -name "$_x" -print0 2>/dev/null | tr '\n\0' '\035\n' \
		| sed -e 's|^\./*||' -e '/^[./]$/d' -e '/^\.\.$/d' -e '/^$/d' | tr '\n\035' '\0\n'; } >"$sffpipe"
}

sff_file_stat()
{
	sffpipe_get_sel
	tr '\n\0' '\035\n' <"$sel" >"$tsel"
	[ ! -s "$tsel" ] && sff_abort

	while IFS= read -r _path; do
		echo ""
		_path=$(printf "%s" "$_path" | tr '\035' '\n')
		stat -x "$_path" 2>/dev/null || stat "$_path"
		file -bi "$_path"
		file -b "$_path"
	done <"$tsel"
	rm -f "$tsel"
	printf "Press Enter to continue "; read -r _x
}

sff_disk_usage()
{
	sffpipe_get_sel
	echo ""
	LC_ALL= xargs -0 du -shc <"$sel" | sort -h
	echo ""
	df -h "$cwd"
	printf "Press Enter to continue "; read -r _x
}

sff_undo()
{
	[ ! -s "$lastop" ] || [ "$(find "$lastop" -mmin +360)" ] && exit 0
	case "$(cat "$lastop")" in
	'new'|'copy'|'duplicate') sff_undo_new;;
	'move'|'rename') sff_undo_move;;
	esac
}

sff_redo()
{
	[ ! -s "$lastop" ] && exit 0
	case "$(cat "$lastop")" in
	'unnew') sff_do_new;;
	'uncopy') sff_do_paste 'copy' 'w';;
	'unmove') sff_do_paste 'move' 'w';;
	'unrename') sff_do_rename;;
	'unduplicate') sff_do_duplicate;;
	esac
}

sff_help()
{
	sed -n '/[#][?][>]/p' "$0" | sed 's/^.*[#][?][>]//' | less
}

run_pl()
{
	_plugin="${sffdir}/plugins/$1"
	[ ! -e "$_plugin" ] && _plugin="${0%/*}/plugins/$1"
	[ ! -e "$_plugin" ] && _plugin="/usr/local/lib/sff/plugins/$1"
	[ ! -e "$_plugin" ] && _plugin="/usr/lib/sff/plugins/$1"
	if [ ! -e "$_plugin" ]; then
		printf "\nPlugin '%s' not found\n" "$1"
		printf "Press Enter to continue "; read -r _x
	else
		unset LC_ALL
		exec "$_plugin" "$sffpipe" "$2"
	fi
}

# === custom functions ===


# === key bindings ===
case "$2" in                  #?> Extension functions:
'dd') sff_delete;;             #?>    dd  Delete
'yy') sff_write_cbuf 'cp';;    #?>    yy  Copy
'dx') sff_write_cbuf 'mv';;    #?>    dx  Cut
'pp') sff_paste 'd';;          #?>    pp  Paste
'pP') sff_paste;;              #?>    pP  Paste and keep buffer
'yY') sff_duplicate;;          #?>    yY  Duplicate
'n') sff_new;;                #?>     n  Create new file (append '/' for dir)
'v') sff_view_cbuf;;          #?>     v  View copy/cut buffer
'V') sff_clear_cbuf;;         #?>     V  Clear copy/cut buffer
'r') sff_rename;;             #?>     r  Rename
'e') sff_edit_file;;          #?>     e  Edit file
'm') sff_chmod_chown;;        #?>     m  Change permissions or owner
'f') sff_find;;               #?>     f  Advanced search via 'find'
'i') sff_file_stat;;          #?>     i  File status
'I') sff_disk_usage;;         #?>     I  Disk usage
'u') sff_undo;;               #?>     u  Undo last operation
'U') sff_redo;;               #?>     U  Redo last operation
'/') sff_help;;               #?>     /  Show this help
                              #?> Plugins:
'F') run_pl 'fzf-find';;      #?>     F  Search via 'fzf'
'=') run_pl 'preview' 'tui';; #?>     =  Toggle preview
'z') run_pl 'archive' 'e';;   #?>     z  Extract archive
'Z') run_pl 'archive' 'c';;   #?>     Z  Create archive
esac                          #?>
                              #?>Use Alt+<Key> or 'u'-<Key> to invoke
                              #?>Press 'q' to leave this page
