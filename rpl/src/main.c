#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#include "shell.h"
#include "net/gnrc/pktdump.h"
#include "net/gnrc/netif.h"
#include "net/sock/udp.h"
#include "xtimer.h"


int send_cmd(int argc, char **argv);
int listen_cmd(int argc, char **argv);

static const shell_command_t shell_commands[] = {
    {"send", "sends measurement payload", send_cmd},
    {"listen", "listens for measurement packets", listen_cmd},
    {NULL, NULL, NULL}
};

static uint8_t l2addr;

int main(void)
{
    /* init message queue */
    msg_t message_queue[8];
    msg_init_queue(message_queue, 8);
    gnrc_netif_t *netif;

    gnrc_pktdump_init();

    netif = gnrc_netif_iter(NULL);
    if (!netif)
        return -EXIT_FAILURE;
    l2addr = netif->l2addr[0];

    printf("BATSIGNAL %02x %u\n", l2addr, netif->pid);


    /* start shell */
    char line_buf[SHELL_DEFAULT_BUFSIZE];
    shell_run(shell_commands, line_buf, SHELL_DEFAULT_BUFSIZE);

    /* should be never reached */
    return EXIT_SUCCESS;
}

/**
 * @brief sends data to specified address
 */
int send_cmd(int argc, char **argv)
{
    int sock;
    struct sockaddr_in6 dst_addr;
    char *payload, *dest;
    int nreps;

    if (argc < 4)
        return -EXIT_FAILURE;

    /* create new socket */
    sock = socket(AF_INET6, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0)
        return -EXIT_FAILURE;

    dest = argv[1];
    nreps = strtol(argv[2], NULL, 10);
    payload = argv[3];

    dst_addr.sin6_family = AF_INET6;
    dst_addr.sin6_port = htons(1337);
    if (inet_pton(AF_INET6, dest, &dst_addr.sin6_addr) < 0)
        return -EXIT_FAILURE;

    for (int i = 0; i < nreps; i++)
    {
        if (sendto(sock, payload, strlen(payload), 0,
                   (struct sockaddr *) &dst_addr,
                   sizeof(struct sockaddr_in6)) < 0)
            return -EXIT_FAILURE;

        printf("SEND %02x\n", l2addr);
        xtimer_usleep(500000);
    }

    return EXIT_SUCCESS;
}

/**
 * @brief opens new udp socket which listens for packets on port 1337
 * 
 * @return @c -EXIT_FAILURE on error.
 * @note this function does block forever
 */
int listen_cmd(int argc, char **argv)
{
    int sock, rv;
    struct sockaddr_in6 src_addr, server_addr;
    char recv_buf[128];
    socklen_t src_len = sizeof(struct sockaddr_in6);

    (void) argc;
    (void) argv;

    /* create new socket */
    sock = socket(AF_INET6, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0)
        return -EXIT_FAILURE;

    server_addr.sin6_family = AF_INET6;
    memset(&server_addr.sin6_addr, 0, sizeof(server_addr.sin6_addr));
    server_addr.sin6_port = htons(1337);

    if (bind(sock, (struct sockaddr *) &server_addr,
             sizeof(server_addr)) < 0)
        return -EXIT_FAILURE;

    /* receive loop */
    while (1)
    {
        if ((rv = recvfrom(sock, recv_buf, 128, 0,
                           (struct sockaddr *) &src_addr, &src_len)) < 0)
            return -EXIT_FAILURE;

        printf("RCV: %02x\n", l2addr);
    }

    return EXIT_SUCCESS;
}
