#include <stdio.h>

#include "shell.h"


static const shell_command_t shell_commands[] = {
    {NULL, NULL, NULL}
};

int main(void)
{
    /* init message queue */
    msg_t message_queue[8];
    msg_init_queue(message_queue, 8);

    /* start shell */
    char line_buf[SHELL_DEFAULT_BUFSIZE];
    shell_run(shell_commands, line_buf, SHELL_DEFAULT_BUFSIZE);

    /* should be never reached */
    return 0;
}
