#include "livesonareas.h"
#include "errorhandler.h"
#include "gadget.h"

extern ErrorHandler handle;

LivesOnAreas::LivesOnAreas(const IntVector& Areas) : areas(Areas) {
  int i, j, maxim = 0;
  for (i = 0; i < areas.Size(); i++)
    if (areas[i] > maxim)
      maxim = areas[i];

  for (i = 0; i < areas.Size(); i++)
    for (j = 0; j < areas.Size(); j++)
      if ((areas[i] == areas[j]) && (i != j))
        handle.logMessage(LOGFAIL, "Error in input files - repeated area", i);

  areaConvert.resize(maxim + 1, -1);
  for (i = 0; i < areas.Size(); i++)
    areaConvert[areas[i]] = i;
}

int LivesOnAreas::isInArea(int area) const {
  if (0 <= area && area < areaConvert.Size())
    return (areaConvert[area] >= 0);
  return 0;
}

int LivesOnAreas::areaNum(int area) const {
  if (0 <= area && area < areaConvert.Size())
    return areaConvert[area];
  return -1;
}

void LivesOnAreas::storeAreas(const IntVector& Areas) {
  int i, j, maxim = 0;
  if (Areas.Size() == 0)
    handle.logMessage(LOGFAIL, "Error in input files - found no areas");

  areas.Reset();
  areaConvert.Reset();
  areas.resize(Areas.Size());
  for (i = 0; i < areas.Size(); i++) {
    areas[i] = Areas[i];
    if (areas[i] > maxim)
      maxim = areas[i];
  }

  for (i = 0; i < areas.Size(); i++)
    for (j = 0; j < areas.Size(); j++)
      if ((areas[i] == areas[j]) && (i != j))
        handle.logMessage(LOGFAIL, "Error in input files - repeated area", i);

  areaConvert.resize(maxim + 1, -1);
  for (i = 0; i < areas.Size(); i++)
    areaConvert[areas[i]] = i;
}
