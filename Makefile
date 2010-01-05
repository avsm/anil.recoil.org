all:
	@cd crunch && $(MAKE) all
	@cd src && $(MAKE) all

clean:
	@cd crunch && $(MAKE) clean
	@cd src && $(MAKE) clean
