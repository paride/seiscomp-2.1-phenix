/*
 * File     :
 *   q330_plugin.h
 *
 * Purpose  :
 *   Declare subroutines used by q330_plugin main routine.
 *
 * Author   :
 *   Phil Maechling
 *   Chad Trabant (q330_plugin modifications)
 *
 * Mod Date :
 *  27 July 2002
 *  1 October 2004, Andres Heinloo
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
 */
#ifndef QMASERV_H
#define QMASERV_H

void initializeSignalHandlers();
void readOrRereadConfigFile();
void resetAll(); 
void tx_cmd();
bool rx_msg();
void assembleBlockettesIntoSecond();
void assembleSecondsIntoPacket();
void check_state();

#endif
