
enum TUP_ERRORS {
  T_OK = 0,
  T_OPEN_DEV_ERROR = -1,
  T_SET_TAP_ERROR = -2,
  T_GET_IF_ERROR = -3,
  T_SET_IF_ERROR = -4,
  T_SET_ADR_ERROR = -5,
  T_SET_MASK_ERROR = -6
};


extern int tun_open(char* name);
extern int tun_up(char* name);
extern int tun_set_ip(char* name, unsigned int ip);
extern int tun_set_mask(char* name, unsigned int mask);
