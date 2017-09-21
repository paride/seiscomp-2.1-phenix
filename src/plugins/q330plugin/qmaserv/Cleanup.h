/*
 * File     :
 *  Cleanup.h
 *
 * Purpose  :
 *  This is the cleanup routine which is called to exit the program.
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *  8 July 2002
 *  1 October 2004, Andres Heinloo
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
 *
 */
#ifndef CLEANUP_H
#define CLEANUP_H

#ifdef __cplusplus
extern "C" 
{
#endif

void AbortQMA(int sig);
void CleanQMA(int sig);
void ShutdownQMA(int sig); 
void ResetQMA(bool flush_queues);
/* void QmaExit(); */
void clearInputQueue();

#ifdef __cplusplus
}
#endif
#endif
