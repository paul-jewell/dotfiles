scp -r paul@10.1.1.112:~/tmp/dotfiles .
cryptsetup luksFormat --type luks1 /dev/nvme0n1p2 --label guix-root
cryptsetup luksOpen /dev/nvme0n1p2 enc
mkfs.btrfs /dev/mapper/enc
mkdir /mnt/ssd && mount /dev/mapper/enc /mnt/ssd

cd /mnt/ssd
echo "...creating subvolumes..."
btrfs subvolume create @
btrfs subvolume create @boot
btrfs subvolume create @home
btrfs subvolume create @gnu
btrfs subvolume create @log
btrfs subvolume create @swap
btrfs subvolume create @data

btrfs filesystem mkswapfile --size 32g --uuid clear @swap/swapfile

echo "...snapshot initial setup"
btrfs subvolume snapshot -r @ blank@

mount -o subvol=@ /dev/mapper/enc /mnt

echo "...mount subvolumes"
cd /mnt && mkdir -p {boot,home,gnu,data,var/log,swap,data}
mount /dev/mapper/enc -o subvol=@boot boot
mount /dev/mapper/enc -o subvol=@data data
mount /dev/mapper/enc -o subvol=@log var/log
mount /dev/mapper/enc -o subvol=@swap swap
mount /dev/mapper/enc -o subvol=@home home
mount /dev/mapper/enc -o subvol=@gnu gnu
echo "...create the boot/efi directory and mount the efi drive: "
mkdir boot/efi
mount /dev/nvme0n1p1 boot/efi
herd start cow-store /mnt

echo "...load channel file and pull"
cd ~/dotfiles
mkdir ~/.config/guix && cp .files/.config/guix/channels.scm ~/.config/guix
guix pull
GUIX_PROFILE="/root/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
hash guix
echo "...Carry out the installation"
guix system init -L . paulj/systems/venus.scm /mnt
echo "...complete."
