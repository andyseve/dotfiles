# Setting up gpg to manage logins.
We can use gpg + pass to manage logins in git, so that I don't have to type in passwords everytime. This uses the helper script `dotfiles/bin/getpass`. For each credential, you need to add a different entry in the user .gitconfig file.

The format usually is

```gitconfig
[credential url]
	username = user
	helper = "!f() { test $1 = get && echo password=`getpass -p pass`;}; f"
```
Here, pass is the name of the corresponding entry in the password store.
