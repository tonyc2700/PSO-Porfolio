/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFTIMER_H
#define PROFTIMER_H

#include "BeginPrivate.h"

void initProfTimer      ( void );
void handleProfTick     ( void );

#ifdef PROFILING
void stopProfTimer      ( void );
void startProfTimer     ( void );
#endif

void stopHeapProfTimer  ( void );
void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;

#include "EndPrivate.h"

#endif /* PROFTIMER_H */
