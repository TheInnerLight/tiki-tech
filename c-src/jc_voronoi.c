#define JC_VORONOI_IMPLEMENTATION
#define JCV_REAL_TYPE double
#define JCV_ATAN2 atan2
#define JCV_SQRT sqrt
#define JCV_FLT_MAX DBL_MAX
#define JCV_PI 3.141592653589793115997963468544185161590576171875
//define JCV_EDGE_INTERSECT_THRESHOLD 1.0e-10F
#define VORONOI_IMPLEMENTATION
#include "jc_voronoi.h"

jcv_diagram* generate_from_points(int numpoints, const jcv_rect* bbox, const jcv_point* points) {
    jcv_diagram* diagram = calloc(1, sizeof(jcv_diagram));
    jcv_diagram_generate(numpoints, points, bbox, 0, diagram );
    return diagram;
}

