  <group
    address="#srcaddr#:#srcport#"
    seqfile="#home_sysop#/status/#statid#.dial.seq"
    lockfile="#home_sysop#/status/dial.pid"
    uptime="#dial_uptime#"
    schedule="#dial_schedule#"
    ifup=""
    ifdown="">

    <station id="#statid#" name="#station#" network="#netid#" selectors="#selectors#"/>
  </group>
