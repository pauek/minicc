BUILD_TARGETS := minicc release debug
OTHER_TARGETS := format clean

all: debug

$(BUILD_TARGETS):
	@$(MAKE) -C src $@
	@cp src/minicc .

$(OTHER_TARGETS):
	@$(MAKE) -C src $@

test: debug
	./test.sh

.PHONY: $(BUILD_TARGETS) $(OTHER_TARGETS)
