################################################################################
# Helper Functinos #############################################################
################################################################################
		
# backs up if the directory exists
# bup <dir>
bup() {
	if [ -e "$1" ] || [ -L "$1" ]; then
		echo "creating backup of $1..."
		mv $1 $1.bak
		if [[ ${OVERWRITE+x}  ]]; then echo "$1" >> $OVERWRITE; fi;
		if [[ ${DID_OVERWRITE+x} ]]; then DID_OVERWRITE=true; fi;
	fi
}
# creates dir if it doesn't exist. 
# cdir <dir>
cdir() {
	if [ ! -d "$1" ]; then
		echo "creating $1..."
		mkdir -p $1
	fi
}
# moves file/directory to new location (moves $1 to $2)
# move <old> <new>
move() {
	if [ -e "$1" ]; then
		if [ -e "$2" ]; then
			echo "oops. $2 already exists, you should look into this"
			bup $2
			mv $1 $2
		else
			echo "moving $1 to $2..."
			mv $1 $2
		fi
	fi
}
# links $1 to $2 if it's not linked already
# link <source> <target>
link() {
	if [ ! "$(readlink $2)" = "$1" ]; then
		bup $2
		echo "creating symlink $2 -> $1..."
		ln -s $1 $2
	fi
}
# checks if command exists and is executable
# check <command>
check() {
	if [ -x "$(command -v $1)" ]; then
		true
	else
		false
	fi
}
checkpkg() {
	if check apt; then
		if dpkg -l | grep -q $1; then
			true
		else
			false
		fi
	fi
}
# echos that command isn't installed
nope() {
	echo -e "$1 is\e[31m not installed\e[0m"
}
# cloning git repo $1 to folder $2
# clone <url> <dir>
clone() {
	if [ -e "$2" ]; then
		echo "oops! directory $2 already exists. Can't clone in there."
	else
		echo "cloning $1 into $2"
		git clone $1 $2
	fi
}
# downloading file with url=$1 to $2
# download <url> <dir>
download() {
	if [ -e "$2" ]; then
		echo "file $2 already exists"
	else
		curl -fLo "$2" --create-dirs "$1"
	fi
}
