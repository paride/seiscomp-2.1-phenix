/*
 * File     :
 *  SplitSOD.h
 *
 * Purpose  :
 *   Create SOD's from from one second of data that contains
 *   more than a packets worth of data.
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *  14 July 2002
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
#ifndef SPLIT_SOD_H
#define SPLIT_SOD_H

#include "QmaLimits.h"
#include "QmaTypes.h"
#include "LCQVO.h"
#include "SecondOfData.h"
#include "qma_mseed.h"

SecondOfData splitSOD(double freqHertz,
                      SecondOfData& sod);

#endif
