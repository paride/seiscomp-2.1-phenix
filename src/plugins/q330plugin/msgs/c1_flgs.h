/*
 *
 * File     :
 *   c1_flgs.h
 *
 * Purpose  :
 *   This is a flags response message.
 *
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *   4 March 2002
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

#ifndef C1_FLGS_H
#define C1_FLGS_H

#include "Field.h"
#include "QmaTypes.h"
#include "QmaLimits.h"
#include "PacketElement.h"

class c1_flgs : public PacketElement
{
  public:

    c1_flgs();
    ~c1_flgs() {};

    //
    // This message has variable length fields.
    // Add special set and get methods to deal with
    // length of message info as received.
    //
    void setBitString(const unsigned char* buf,qma_uint32 len);
    unsigned char* getOffsetBitString(const qma_uint16 start) const;

    //
    // Operations on fields
    //
   
    qma_uint16 getOffsetToFixedValues() const;
    qma_uint16 getOffsetToGlobalProgramming() const;
    qma_uint16 getOffsetToSensorControl() const;
    qma_uint16 getOffsetToDataPort() const;

    Field   p_fieldList[FIELDS_IN_C1_FLGS];

};

#endif
