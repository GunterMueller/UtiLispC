#! /usr/local/bin/perl

do 'sys/socket.ph' || die "Can't do sys/socket.h: $!\n";

$sockaddr_in = 'S n a4 x8';
$sockaddr_un = 'S a108';
$DISPLAY = $#ARGV == -1 ? $ENV{'DISPLAY'} : $ARGV[0];
($server_host, $display, $screen) = split(/[:.]/, $DISPLAY);
die "server unknown\n" unless $server_host;
$display = 0 unless $display;
#$server_host eq 'unix' && die "No support for unix domain socket\n";
print "server: $server_host, display: $display\n";

if($server_host eq 'unix'){
  $server_unix = "/tmp/.X11-unix/X$display";
  $server_addr = pack($sockaddr_un,&AF_UNIX, $server_unix);
  print "addr: $server_addr\n";
  socket(S, &PF_UNIX, &SOCK_STREAM, 0) || die "socket: $!\n";
  connect(S, $server_addr) || die "connect: $!\n";
}
else{
  $server_inet = (gethostbyname($server_host))[4];
  $server_addr = pack($sockaddr_in,&AF_INET, 6000 + $display, $server_inet);
  socket(S, &AF_INET, &SOCK_STREAM, 0) || die "socket: $!\n";
  connect(S, $server_addr) || die "connect: $!\n";
}
select(S);$|=1;select(stdout);

$setuppacket = 'cxs4x2';
$packet = pack($setuppacket, 0x42, 11, 0, 0, 0);
print S $packet;

read(S, $packet, 8);
($success, $reasonlen, $mjv, $miv, $datalen)
	= unpack('c2s3', $packet);

print "version: $mjv.$miv\n";

if($success == 0){
  print "Connection failed.\n";
  read(S, $packet, $reasonlen);
  print $packet;
  exit 0;
}
else{
  print "Connection succeeded\n";
  read(S, $packet, $datalen * 4);
}

$packet = pack('cxs', 110, 1);
print S $packet;
read(S, $packet, 8);
($code, $mode, $seq, $len) = unpack('c2sI', $packet);
print "$code: mode = $mode\n";
print "sequence #: $seq\n";
print "replylen: $len\n";

