### dotfiles

clone into home directory and then `makeSymLinks.sh` to symmlink all files to home directory
if the file already exists, it will be moved to a directory called `dotfiles_old`

#### setup dotfiles on machine

    git clone https://github.com/cole-brokamp/dotfiles
    cd ~/dotfiles
    ./makeSymLinks.sh
