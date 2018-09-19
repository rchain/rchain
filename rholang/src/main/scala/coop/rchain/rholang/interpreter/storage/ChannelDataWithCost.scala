package coop.rchain.rholang.interpreter.storage

import coop.rchain.models.ListChannelWithRandom
import coop.rchain.rholang.interpreter.accounting.CostAccount

final case class ChannelDataWithCost(dataList: ListChannelWithRandom, cost: CostAccount)
