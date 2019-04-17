# Copies the pathname of the current directory to the system or X Windows clipboard
# cpdclip <dir>
function cpdclip {
  emulate -L zsh
  print -n $PWD | clipcopy
}
