#!/usr/bin/env -S sh
# -*- Mode:Shell-script; Coding:us-ascii-unix; fill-column:158 -*-

doxygen
rsync -rlt --log-format=%f --stats --delete --delete-excluded --modify-window=2 ~/MJR/world/my_prog/finance/FortranFinance/MRFFL/autodocs/html/ ~/MJR/WWW/site/SS/FortranFinance/MRFFL_API_Docs
