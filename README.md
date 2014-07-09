journal-mailer
==============

daemon that sends out email notifications on error messages in systemd's journal


Currently `systemd` and `journald` have no automatic way of sending mail notifications
on failing systemd units or other error conditions. `journal-mailer` is a tool that
listens in on all journal messages and sends out notification mails for messages
of log level `LOG_ERR` or worse.
