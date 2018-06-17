# SUN 3 Makefile
.PRECIOUS:	mdb
CFLAGS=	-g -DM68=1 -DM68010=0 -DM68020=1 -DSUNOS4 -DVERSION=\"2.3\"
LIBS=	
OBJS=	mdb_access.o mdb_aout.o mdb_brkpt.o mdb_core.o mdb_corewin.o \
	mdb_lcmd.o mdb_data.o mdb_fast.o mdb_ln.o mdb_main.o mdb_map.o \
	m68k-pinsn.o \
	mdb_modules.o mdb_path.o mdb_process.o \
	mdb_ref.o mdb_stack.o mdb_text.o mdb_tree.o mdb_error.o \
	mdb_instr.o mdb_watch.o \
	copying.o mdb_copying.o
D_AOUT=	mdb_aout.o mdb_main.o

mdb:	${OBJS}
	cc ${CFLAGS} ${OBJS} ${LIBS}
	cp a.out mdb

debug:	; make 'CFLAGS=-DDEBUG_AOUT' d_aout

d_aout:	${D_AOUT}
	cc -o d_aout ${CFLAGS} ${D_AOUT}

oh:	; oh *.c <makefile >/tmp/oh$$$$ && cp /tmp/oh$$$$ makefile; \
	rm -f /tmp/oh$$$$

clean:	; rm -f ${OBJS}

copying.c:	COPYING copying.awk
		tr -d '\014' <COPYING | \
		sed 's/"/\\"/g' | awk -f copying.awk >copying.c

#oh
mdb_access.o:	mdb_ptrace.h mdb.h mdb_config.h
mdb_aout.o:	mdb.h mdb_config.h mdb_ref.h mdb_tree.h mdb_bp.h mdb_map.h
mdb_brkpt.o:	mdb_ref.h mdb_tree.h mdb.h mdb_config.h mdb_bp.h
mdb_cmd.o:	mdb.h mdb_config.h mdb_ref.h mdb_bp.h mdb_tree.h mdb_procstr.h mdb_view.h
mdb_core.o:	mdb.h mdb_config.h mdb_map.h mdb_user.h
mdb_corewin.o:	mdb.h mdb_config.h mdb_ref.h mdb_tree.h mdb_bp.h mdb_view.h
mdb_data.o:	mdb_ref.h mdb_bp.h mdb.h mdb_config.h mdb_tree.h mdb_view.h mdb_procstr.h
mdb_error.o:	mdb_error.h mdb_instr.h mdb.h mdb_config.h
mdb_fast.o:	mdb_ref.h mdb_tree.h mdb.h mdb_config.h mdb_view.h
mdb_instr.o:	mdb.h mdb_config.h mdb_ref.h mdb_tree.h mdb_view.h
mdb_lcmd.o:	mdb.h mdb_config.h mdb_ref.h mdb_bp.h mdb_tree.h mdb_procstr.h mdb_view.h mdb_map.h
mdb_ln.o:	mdb.h mdb_config.h
m68k-pinsn.o:	mdb_ref.h mdb.h mdb_tree.h m68k-opcode.h
mdb_main.o:	mdb.h mdb_config.h
mdb_map.o:	mdb.h mdb_config.h mdb_map.h
mdb_modules.o:	mdb.h mdb_config.h mdb_ref.h mdb_bp.h mdb_tree.h mdb_view.h
mdb_opset.o:	mdb.h mdb_config.h mdb_instr.h mdb_ref.h mdb_tree.h
mdb_path.o:	mdb.h mdb_config.h
mdb_pe3200.o:	mdb.h mdb_config.h mdb_instr.h mdb_ref.h mdb_tree.h
mdb_process.o:	mdb.h mdb_config.h mdb_ref.h mdb_tree.h mdb_bp.h mdb_ptrace.h mdb_user.h
mdb_ref.o:	mdb_ref.h mdb_tree.h mdb.h mdb_config.h
mdb_stack.o:	mdb.h mdb_config.h mdb_ref.h mdb_tree.h mdb_bp.h mdb_view.h mdb_procstr.h mdb_instr.h
mdb_text.o:	mdb_ref.h mdb_tree.h mdb.h mdb_config.h mdb_bp.h mdb_view.h
mdb_tree.o:	mdb_ref.h mdb_bp.h mdb.h mdb_config.h mdb_tree.h
mdb_watch.o:	mdb.h mdb_config.h
