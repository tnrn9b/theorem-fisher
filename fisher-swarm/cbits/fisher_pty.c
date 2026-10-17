/* Terminal size and raw mode, which System.Posix does not expose. */
#include <stdlib.h>
#include <sys/ioctl.h>
#include <termios.h>

int fisher_get_winsize(int fd, int *rows, int *cols) {
  struct winsize ws;
  if (ioctl(fd, TIOCGWINSZ, &ws) != 0) return -1;
  *rows = ws.ws_row;
  *cols = ws.ws_col;
  return 0;
}

int fisher_set_winsize(int fd, int rows, int cols) {
  struct winsize ws = {0};
  ws.ws_row = (unsigned short) rows;
  ws.ws_col = (unsigned short) cols;
  return ioctl(fd, TIOCSWINSZ, &ws);
}

/* Put fd in raw mode (as Python's tty.setraw) and return the old settings,
   or NULL if fd is not a terminal. */
void *fisher_make_raw(int fd) {
  struct termios *saved = malloc(sizeof *saved);
  if (!saved) return NULL;
  if (tcgetattr(fd, saved) != 0) { free(saved); return NULL; }
  struct termios raw = *saved;
  cfmakeraw(&raw);
  if (tcsetattr(fd, TCSAFLUSH, &raw) != 0) { free(saved); return NULL; }
  return saved;
}

void fisher_restore(int fd, void *saved) {
  if (!saved) return;
  tcsetattr(fd, TCSAFLUSH, (struct termios *) saved);
  free(saved);
}
