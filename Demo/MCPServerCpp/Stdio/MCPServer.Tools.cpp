#include <System.hpp>

#include "MCPServer.Tools.h"

#pragma hdrstop

int __fastcall TRegisterToolTest::RandomNumber(int AMax)
{
    if (AMax <= 0)
        return 0;

    return System::Random(AMax);
}
