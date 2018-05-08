sudo apt-get -y install build-essential
sudo apt-get -y remove --purge gconf2 && sudo apt-get install gconf2
wget https://downloads.slack-edge.com/linux_releases/slack-desktop-3.1.1-amd64.deb
sudo dpkg -i slack-desktop-3.1.1-amd64.deb
wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
sudo apt-get -y install apt-transport-https
echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
sudo apt-get update
sudo apt-get -y install sublime-text
