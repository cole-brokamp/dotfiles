#### Update `$HOSTNAME`

- `sudo nano /etc/hostname` and change to desired computer name
- `sudo nano /etc/hosts` and add new line `127.0.1.1 new-hostname`
- computer must be rebooted for changes to take effect (`sudo reboot`)
- don't forget: only alphanum, ".", and "-" are valid!
