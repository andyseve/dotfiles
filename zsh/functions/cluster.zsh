# Author: Anish Sevekari
# Last Modified: Fri 12 Jul 2019 03:21:40 PM EDT
# File to provide utils for cluster access

function cluster(){
	#if [[ -v VALUES ]]; then
		#echo "what the fuck"
		#continue;
	#else
		typeset -A VALUES;
		VALUES[s]=fp81 # server
		VALUES[d]=math.cmu.edu # domain name
		VALUES[n]=${VALUES[s]}.${VALUES[d]}
		VALUES[t]=matlab
		VALUES[c]=sync
	#fi

	SET_VALUE=0
	for var in "$0"; do
		if [[ SET_VALUE == 0 ]]; then
			if [[ var == --* ]]; then
				SET_VALUE=${var:2:2}
			elif [[ var == -* ]]; then
				SET_VALUE=${var:1:1}
			else
				cluster_help();
			fi
		elif [[ SET_VALUE == 1 ]]; then
			VALUES["$SET_VALUE"]=$var;
		else
			cluster_help();
		fi
	done
}

function cluster_help(){
	echo "some error in usage, try again";
}
