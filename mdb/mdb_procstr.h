/*
 *	structure of a Modula-2 process
 *
 *	see rts/newprocess.s or rts/transfer.s
 */

struct process {
#if PE3200
       int p_base;
       int p_top;
       int p_limit;
#endif
#if M68
       int p_limit;
       int p_base;
       int p_top;
#endif
       int p_pc;
       int p_started;
};
