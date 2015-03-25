#ifndef FGBC_H
#define FGBC_H

void fgbc_debugger_break();
void fgbc_debugger_stop();
void fgbc_print_string(const char*);
void fgbc_print_int(int);

#define prints fgbc_print_string
#define printsline(string) fgbc_print_string(string); fgbc_print_string("\n")

#define printi fgbc_print_int
#define printiline(i) fgbc_print_int(i); fgbc_print_string("\n")

#define DEBUGGER fgbc_debugger_break();

#define STOP fgbc_debugger_stop();

#endif