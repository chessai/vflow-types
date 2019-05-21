# vflow-types

[![Hackage](https://img.shields.io/hackage/v/vflow-types.svg)](https://hackage.haskell.org/package/vflow-types)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)


`vflow-types` provides types suitable for ingesting vflow data with aeson.

Verizon Digital's [vflow](https://github.com/VerizonDigital/vflow) is a network flow collector. Features:

  * IPFIX RFC7011 collector
  * sFlow v5 raw header / counters collector
  * Netflow v5 collector
  * Netflow v9 collector
  * Decoding sFlow raw header L2/L3/L4
  * Producer to Apache Kafka, NSQ, NATS
  * Replicate IPFIX to 3rd party collector
  * Support for IPv4 and IPv6
  * Monitoring with InfluxDB and OpenTSDB backend
  * Easy integration with JUNOS

Note that this is not an official Verizon Digital product.

