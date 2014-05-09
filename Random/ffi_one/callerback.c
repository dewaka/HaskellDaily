#include "callerback.h"

double twice(d2d f, double x) {
    return f(f(x));
}
