#ifndef MODULEF_H
#define MODULEF_H

#include <stdbool.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "error.h"
#include "file.h"
#include "auxiliar.h"

ERROR freq_rle(char** path, bool doRLE, bool doFREQ, unsigned long size);

#endif