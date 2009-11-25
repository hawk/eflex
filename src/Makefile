export ERL_LIBS := /opt/local/pgm/erlang-extra-R12B/lib

ESRC = .
EGEN = gen
EBIN = ../ebin
ERLC = erlc
ERL_PATH =
ERL_FLAGS = -smp
#-pa /opt/local/pgm/otp_R12B-5/lib/erlang/lib/wx-0.97.1119/ebin
ERL_COMPILE_FLAGS = +warn_unused_vars +debug_info 
EFLEX_BOOT_FLAGS=-pa ../../eflex/ebin -boot start_sasl -sasl errlog_type error

ERL_MODULES = \
		eflex \
                eflex_break \
                eflex_flextool \
                eflex_lib \
                eflex_sup \
                eflex_wx \
                eflex_xml

ERL_SOURCES = $(ERL_MODULES:%=%.erl)
ERL_TARGETS = $(ERL_MODULES:%=$(EBIN)/%.beam)

APP_FILE   = eflex.app
APP_SOURCE = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

ALL_TARGETS = $(ERL_TARGETS) $(APP_TARGET)
# Targets
all: $(ALL_TARGETS)

$(APP_TARGET): $(APP_SOURCE)
	cp  $< $@

clean:  
	rm -f $(ALL_TARGETS)
	rm -f erl_crash.dump core* *~

win:  $(ALL_TARGETS)
	erl $(ERL_FLAGS) $(EFLEX_BOOT_FLAGS) -s eflex

windebug:  $(ALL_TARGETS)
	erl $(ERL_FLAGS) $(EFLEX_BOOT_FLAGS) -s eflex debug

nowin:  $(ALL_TARGETS)
	erl $(ERL_FLAGS) $(EFLEX_BOOT_FLAGS) -s eflex batch

work:  $(ALL_TARGETS)
	erl $(ERL_FLAGS) $(EFLEX_BOOT_FLAGS)

# Rules
$(EBIN)/%.beam: $(ESRC)/%.erl eflex.hrl
	$(ERLC) -W -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $< 
