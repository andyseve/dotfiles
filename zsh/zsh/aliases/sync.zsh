# Sync to cmu laptop
alias booksync='unison /home/stranger/Documents/Books\ and\ Papers ssh://asevekarlaptop.math.cmu.edu:2718//home/stranger/Documents/Books\ and\ Papers -perms=0'
alias bashsync='unison /home/stranger/.bash_aliases ssh://asevekarlaptop.math.cmu.edu:2718//home/stranger/.bash_aliases'
alias tasksync='task sync && read -n 1 -s && clear && task next'
