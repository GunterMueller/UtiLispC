#! /usr/local/bin/perl

do 'sys/socket.ph' || die "can't do sys/socket.ph...\n";
# fakeserver.pl
# fakes X server

$|=1;
$port = 6001	# as if this server is display 1 (second display)

# sockaddr_in
$sockaddr = 'Sna4x8';
chop($hostname = `hostname`);
$myipaddr = (gethostbyname($hostname))[4];

$mysockaddr = pack($sockaddr,&AF_INET,$port,$mayipaddr);

socket(S, &PF_INET, &SOCK_STREAM, 0) || die "socket: $!\n";
select(S); $|=1; select(stdout);
bind(S, $mysockaddr) || die "bind: $!\n";
listen(S, 5) || die "listen: $!\n";

loop: ($addr = accept(NS, S) || goto loop;
($af, $port, $inetaddr) = unpack($sockaddr, $addr);
print "connected: port = $port addr = $inetaddr\n";

$setuppacket = 'cx'
