#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <netinet/ip.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>

#include "vpn_tun.h"

int
tun_open(char* name){
  struct ifreq ifr;
  int fd = open("/dev/net/tun", O_RDWR);
  
  if (fd < 0) {
    return T_OPEN_DEV_ERROR;
  }

  memset(&ifr,0,sizeof(ifr));
  strncpy(ifr.ifr_name, name, IFNAMSIZ-1);

  ifr.ifr_flags = IFF_TUN | IFF_NO_PI;
  //  ifr.ifr_flags = IFF_TUN // | IFF_NO_PI;
  if (ioctl(fd, TUNSETIFF, (void *)&ifr) < 0) {
    return T_SET_TAP_ERROR;
  }
  return fd;
}

int
tap_open(char* name){
  struct ifreq ifr;
  int fd = open("/dev/net/tun", O_RDWR);
  
  if (fd < 0) {
    return T_OPEN_DEV_ERROR;
  }

  memset(&ifr,0,sizeof(ifr));
  strncpy(ifr.ifr_name, name, IFNAMSIZ-1);

  ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
  if (ioctl(fd, TUNSETIFF, (void *)&ifr) < 0) {
    return T_SET_TAP_ERROR;
  }
  return fd;
}


int
if_up(char* name){
  int fd;
  struct ifreq ifr;
  
  fd = socket(AF_INET, SOCK_DGRAM, 0);
  strncpy(ifr.ifr_name,name, IFNAMSIZ-1);
  
  if (ioctl(fd, SIOCGIFFLAGS, &ifr) != 0) {
    close(fd);
    return T_GET_IF_ERROR;
  }

  ifr.ifr_flags |= (IFF_UP | IFF_RUNNING);

  if (ioctl(fd, SIOCSIFFLAGS, &ifr) < 0){
    close(fd);
    return T_SET_IF_ERROR;
  }
  close(fd);
  return T_OK;
}

int
if_set_ip(char* name, unsigned int ip){
  struct sockaddr_in addr;
  struct ifreq ifr;
  int fd;

  fd = socket(AF_INET, SOCK_DGRAM, 0);

  strncpy(ifr.ifr_name,name, IFNAMSIZ-1);
  
  if (ioctl(fd, SIOCGIFFLAGS, &ifr) != 0) {
    close(fd);
    return T_GET_IF_ERROR;
  }

  addr.sin_addr.s_addr = ip;
  addr.sin_family = AF_INET;
  memcpy(&ifr.ifr_addr, &addr, sizeof(addr));

  if (ioctl(fd, SIOCSIFADDR, &ifr) < 0) {
    close(fd);
    return T_SET_ADR_ERROR;
  }
  close(fd);
  return T_OK;
}

int
if_set_mask(char* name, unsigned int mask){
  struct sockaddr_in addr;
  struct ifreq ifr;
  int fd;

  fd = socket(AF_INET, SOCK_DGRAM, 0);

  strncpy(ifr.ifr_name,name, IFNAMSIZ-1);
  
  if (ioctl(fd, SIOCGIFFLAGS, &ifr) != 0) {
    close(fd);
    return T_GET_IF_ERROR;
  }

  addr.sin_addr.s_addr = mask;
  addr.sin_family = AF_INET;
  memcpy(&ifr.ifr_addr, &addr, sizeof(addr));

  if (ioctl(fd, SIOCSIFNETMASK, &ifr) < 0) {
    close(fd);
    return T_SET_MASK_ERROR;
  }
  close(fd);
  return T_OK;
}

