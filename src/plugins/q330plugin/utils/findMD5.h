/*
 * Program: Mountainair
 *
 *
 *
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it with the sole restriction that:
 * You must cause any work that you distribute or publish, that in
 * whole or in part contains or is derived from the Program or any
 * part thereof, to be licensed as a whole at no charge to all third parties.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 */
#ifndef FINDMD5_H
#define FINDMD5_H

void findMD5(unsigned long long cv,
	unsigned long serverip,
	unsigned short udpport,
	unsigned short regnum,
	unsigned long long authcode,
	unsigned long long sn,
	unsigned long long rand,
	unsigned char *result);
#endif
