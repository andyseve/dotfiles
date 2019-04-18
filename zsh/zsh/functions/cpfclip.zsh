# copies the contents of a given file to the system or X Windows clipboard
# cpfclip <file>
function cpfclip {
  emulate -L zsh
  clipcopy $1
}
