#!/usr/bin/env python3

from argparse import Parser

from anaranth import *
from amaranth.lib import wiring
from amaranth.lib.wiring import In, Out

from amaranth.sim import Simulator as simu

class UpCounter(wiring.Component):
    en: In(1)
    ovf: Out(1)

    def __init__(self, limit):
        super().__init__()
        self.limit = limit
        self.count = Signal(16)

    def elaborate(self, platform):
        m = Module()

        m.d.comb + self.ovf.eq(self.count == self.limit)

        with m.If(self.en):
            with m.If(self.ovf):
                m.d.sync += self.count.eq(0)
            with m.Else():
                m.d.sync += self.count.eq(self.count + 1)

        return m
