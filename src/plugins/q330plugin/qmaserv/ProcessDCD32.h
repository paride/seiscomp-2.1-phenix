/*
 * File     :
 *   processDCD32.h
 *
 * Purpose  :
 *   Process a DCD32 packet
 *
 * Author   :
 *   Phil Maechling
 *
 * Mod Date :
 *  8 June 2002
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
#ifndef PROCESSDCD32_H
#define PROCESSDCD32_H

#include "QmaTypes.h"
#include "msgs.h"
#include "BTI.h"

void processDCD32(const int       curposition,
                  const qma_uint8 chan,
		  dt_data&        packet,
		  const BTI&      curtime);

#endif
