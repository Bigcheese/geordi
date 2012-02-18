#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <map>
#include <cstdlib>
#include <cstdio>
#include <mcheck.h>
#include "geordi.hpp"

extern "C"
{
  void abort() throw() // default version causes "SYS_gettid: Operation not permitted".
  {
    std::printf("%s%s", geordi::parsep, strsignal(SIGABRT));
    std::fclose(stdout); // Prevents things like tracked reporting leaks.
    std::exit(0);
  }
}
