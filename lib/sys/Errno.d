DEFINITION MODULE Errno;

   CONST 
   (* available on System V *)
      EPERM   = 1;         EACCES  = 13;        ENOTTY  = 25;  
      ENOENT  = 2;         EFAULT  = 14;        ETXTBSY = 26;  
      ESRCH   = 3;         ENOTBLK = 15;        EFBIG   = 27;  
      EINTR   = 4;         EBUSY   = 16;        ENOSPC  = 28;  
      EIO     = 5;         EEXIST  = 17;        ESPIPE  = 29;  
      ENXIO   = 6;         EXDEV   = 18;        EROFS   = 30;  
      E2BIG   = 7;         ENODEV  = 19;        EMLINK  = 31;  
      ENOEXEC = 8;         ENOTDIR = 20;        EPIPE   = 32;  
      EBADF   = 9;         EISDIR  = 21;        EDOM    = 33;  
      ECHILD  = 10;        EINVAL  = 22;        ERANGE  = 34;  
      EAGAIN  = 11;        ENFILE  = 23;
      ENOMEM  = 12;        EMFILE  = 24;  
      ENOMSG  = 75(*35 on SYSV*);        EIDRM   = 77(*36*);
      (*EDEADLOCK = 45;    	not available on SUN *)
  (* available on SUN*)

      EWOULDBLOCK = 35;		 EINPROGRESS = 36;
      EALREADY = 37;		 ENOTSOCK = 38;
      EDESTADDRREQ = 39;	 EMSGSIZE = 40;
      EPROTOTYPE = 41;		 ENOPROTOOPT = 42;
      EPROTONOSUPPORT = 43;	 ESOCKTNOSUPPORT = 44;
      EOPNOTSUPP = 45;		 EPFNOSUPPORT = 46;
      EAFNOSUPPORT = 47;	 EADDRINUSE = 48;
      EADDRNOTAVAIL = 49;	 ENETDOWN = 50;
      ENETUNREACH = 51;		 ENETRESET = 52;
      ECONNABORTED = 53;	 ECONNRESET = 54;
      ENOBUFS = 55; 		 EISCONN = 56;
      ENOTCONN = 57; 		 ESHUTDOWN = 58;
      ETIMEDOUT = 60;		 ECONNREFUSED = 61;
      ELOOP = 62; 		 ENAMETOOLONG = 63;
      EHOSTDOWN = 64; 		 EHOSTUNREACHED = 65;
      ENOTEMPTY = 66; 		 EDQUOT = 69;
      ESTALE = 70; 		 EREMOTE = 71;
   VAR errno: CARDINAL;

END Errno. 
