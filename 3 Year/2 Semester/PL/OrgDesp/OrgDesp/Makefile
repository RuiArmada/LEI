install: man/org_desp.l
	@cp -r src /opt/org_desp
	@ln -s -b /opt/org_desp/org_desp.py /usr/local/bin/org_desp
	@cp man/org_desp.l tmp.l
	@gzip tmp.l
	@cp tmp.l.gz /usr/share/man/man1/org_desp.1.gz
	@mandb
	@rm tmp.l.gz

remove:
	@rm -r /opt/org_desp
	@rm /usr/local/bin/org_desp
	@rm /usr/share/man/man1/org_desp.1.gz