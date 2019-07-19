# Author: Anish Sevekari
# Last Modified: Wed 17 Jul 2019 03:37:54 PM DST
# File to provide utils for cluster access

function cluster_help(){
	cat .zsh/functions/cluster.txt;
}

function cluster_info(){
}

function cluster_set_name(){
	if [[ -z ${VALUES["domain"]} ]]; then
		VALUES["name"]=${VALUES["server"]};
	else
		VALUES["name"]=${VALUES["server"]}.${VALUES["domain"]};
	fi
}

function cluster_set(){
	if [[ "$1" == "name" ]]; then
		VALUES["name"]=$2;
		VALUES["server"]=${VALUES["name"]%%.*};
		VALUES["domain"]=${VALUES["name"]#*.};
		return 0;
	fi
	VALUES[$1]=$2;
	cluster_set_name;
}

function cluster(){
	set_value=0;
	configure=0;
	while ! [ -z $1 ]; do
		if [[ $1 == "help" ]]; then
			cluster_help;
			return 0;
		fi
		if [[ $1 =~ '(-|--)?(config)(u|ur|ure)?' ]]; then
			configure=1;
			shift;
			continue;
		fi
		if [[ $set_value == 0 ]]; then
			case "$1" in
				--*) set_value=${1##--};;
				-n) set_Value="name";;
				-s) set_Value="server";;
				-d) set_value="domain";;
				-u) set_value="user";;
				-h) set_value="home";;
				-t) set_value="type";;
				*) cluster_help; return 1;;
			esac
			valid_args=(ssh rsync name server domain user home type);
			if ! [[ -n "${valid_args[(r)$set_value]}" ]]; then
				echo "wrong value set";
				cluster_help;
				return 1;
			fi
		elif ! [[ $set_value == 0 ]]; then
			cluster_set $set_value $1;
		else
			break;
		fi
		shift;
	done
	if [[ $configure == 1 ]]; then
		return 0;
	fi

	# Actual Command Settings
	if [[ -z "$1" ]]; then
		echo "Command not found";
		cluster_help;
		return 1;
	fi

	cmd=$1; shift;
	src=$@;

	# Core of the code, actual functions to handle cluster transfers
	# TODO: implement
	case "$cmd" in
		sync)
			;;
		delete)
			;;
		run)
			;;
		attach)
			;;
		*)
			echo "command not found";
			cluster_help;
			return 1;
			;;
	esac
}

# # Initialization
if [[ -n ${+VALUES} ]]; then
	typeset -A VALUES;
	VALUES["server"]=localhost; # server
	VALUES["domain"]=""; # domain name
	VALUES["user"]=$USER;
	VALUES["home"]=$HOME;
	VALUES["type"]=matlab;
	cluster_set_name;
fi

