package com.revdefine.tools.initialize

import com.revdefine.node.web.transfer.Transfer.{CustomType, Transaction}

object SpecialCases {
  val timestampAt908400   = 1626564356794L
  val blockHashAt908400   = "4628fd789319c66a42d228fe3c87dcc822732a1d6c726d829870570edb372d05"
  val blockNumberAt908400 = 908400L
  val emptyReason         = "empty"
  val perValidatorCustomMes =
    "Hard Fork at 908400 hand-made transfer from perValidator to Coop MultiSig"
  val posStakingCustomMes = "Hard Fork at 908400 hand-made transfer from pos Vault to Coop MultiSig"
  val ethMultiSigRhocCustomMes =
    "Hard Fork at 908400 hand-made transfer from eth contract address to Coop MultiSig2"

  private def toTransaction(transferMes: (String, String, Long), customMes: String) = Transaction(
    transactionType = CustomType(customMes),
    fromAddr = transferMes._1,
    toAddr = transferMes._2,
    amount = transferMes._3,
    blockHash = blockHashAt908400,
    blockNumber = blockNumberAt908400,
    deployId = blockHashAt908400,
    timestamp = timestampAt908400,
    isFinalized = true,
    isSucceeded = true,
    reason = emptyReason
  )

  // commit Debit PoS per validator vaults and crediting Coop MultiSig vault.
  // https://github.com/rchain/rchain/commit/f5d332469942181aa1d3749a89ef0a9815ed7444
  val perValidatorCases: Seq[Transaction] = List(
    (
      "1111oYt5g3EKfuizbJUrFunhxdmowFQUXzrDki2qFQ8uunFmQwqSm",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      17414633L
    ),
    (
      "11112RsUiYwzGsM4C1w1KXzqLZUK4WcEWiZueviiJhWriHkSmNpTza",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      8699689L
    ),
    (
      "11112eeX28rcEGzZ9CjZAeyanvQAop7n9siE8zSep4BiLG6NMvhs9Z",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      12444641L
    ),
    (
      "11112cX59qJxUURdFgQepSuNC2CpV5AZ6FsrtMZpTbXUNFjhQiNRZc",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      21835883L
    ),
    (
      "11112en8X2AryCGtakTQvokzpEurgWfAH6CzjHPg3UTWbrBvQjkR9g",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      7698085L
    ),
    (
      "1111LowRZnbRzU3uGZ5GVMAcZigLSWsxqaRydyujkK8drU2iggUbf",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      9573306L
    ),
    (
      "11112Rb5XSQJeuVeTuP8ieSQqucS7rUUxio2SrGyCa7Bj6qaiEbnB7",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      11307059L
    ),
    (
      "11112SM2Fi38J9fdgCvujzg4DZaYrQYSHSt9cZPwsMQiB6jp2fU7Cd",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      5881096L
    ),
    (
      "1111iHcepxeET3XqwR3a177ZyEbVTN55pDypGdDSnvf4dWvZ4Ws3m",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      5984593L
    ),
    (
      "1111gLbVCFKvgdHx3WXfCsPPu9rc6QK982Di8Ky2mrCm3KnrAF4Kw",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      13356810L
    ),
    (
      "11113KWXYsxJivtMKGf3LW2js9GfK3e5vdL59aerHqm1Bc9t1juKr",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      12828117L
    ),
    (
      "1111e5UJH7AwRQRDxBhi5nRmjg6nRzhN99WTBKaRerW5h6s8X3mqN",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      9864555L
    ),
    (
      "11112MW6m6rbew1imHqcLLRsbEg8M1tRZBXcF8vGJABjjbYo3iPcC5",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      560355956L
    ),
    (
      "11119LqBoAQP7zV3LXXbjRL7kaR1DLWSfn2xGsvPxTw6zh5weNFBN",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      16780791L
    ),
    (
      "1111TbBXzp2atjrpSpD641i5R1MZdGHqPaMT7UFibASvaNWvpdGEK",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      12189171L
    ),
    (
      "1111PTguwCwfLDKfa51oZJqhs1szwcuenApEkvykJp3Q4Y6d79ypJ",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      9053930L
    ),
    (
      "1111GUeqjdNVp7oDDwuo8kUMRjiCZ4qWkatMGsgvffyCrJxVZZikL",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      6437151L
    ),
    (
      "1111YHTd2CNaSEKBrAU4vjeVFZTzew4LTJ2YgGcQJkPNfnRhrvhUW",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      12398450L
    ),
    (
      "11112QkNjD95s6i3MstxsYcPHC4qx5QGVTrXcdbSAB4XmqoL2N3VFW",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      12536281L
    ),
    (
      "111122HCfMu4STPqspYmEwrh6VpeHW6PtB23iuh9EzvzzAw63mEAy7",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      5438193L
    ),
    (
      "1111CyFrj85AwWtZzenh6Q29C1q8U3UmcZ4pvADrSY89bt4VFEjnj",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      15022160L
    ),
    (
      "1111k2zMZbqcV4yNua6YQgUiCxRBubogkFzK6UrSKNCGXRKoGSAK3",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      12806931L
    ),
    (
      "1111244BKRXnje2hXXZaVZGVFmMys9n6F2qN24VNRtwSEdLkmnaU5k",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      39516188L
    ),
    (
      "11112kmj2qDxWLUDMfpZf1MAa62m7hoiUTXnrexyqvZC7Dpi1V9ZtU",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      8675809L
    ),
    (
      "11112UyJvNyopZ9BPktzskA7fV4mZLCvv4k3jW22cssHDGdyuN21zH",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      5990451L
    ),
    (
      "1111yjVxMb3Dca3L8HyH7QDPZL2EKeBDG7PWgMHvBFmTj2WAQDF6B",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      10893716L
    ),
    (
      "11112SWxWtZUDQ1SntUoGVPQeSTzVuWcoyP2P7R7vJgmaz1uVaGxi1",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      14663639L
    ),
    (
      "1111uY3tSgMz9cj8a6Y4Gr8TrrYyhD3zu8BFRH7qg2wGPkafopknb",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      10275075L
    ),
    (
      "1111APUCMaMS8LryGEdtGLSWaQ18VCg81ehyAx1kjFbfgiZpL8v6k",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      8878183L
    ),
    (
      "11112otedAe9yYeeLJoxc4gKBifKdFj4SjxxmoaFjoPFYLc9U8NU1f",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      9055821L
    )
  ).map(toTransaction(_, perValidatorCustomMes))

  // Debit PoS staking vault and crediting Coop MultiSig vault.
  // https://github.com/rchain/rchain/commit/5841812884f02f451134902f7d7ac501f2fc1d27
  val coopCase: Seq[Transaction] = List(
    (
      "1111gW5kkGxHg7xDg6dRkZx2f7qxTizJzaCH9VEM1oJKWRvSX9Sk5",
      "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF",
      1650001916789431L
    )
  ).map(toTransaction(_, posStakingCustomMes))

  // Sweep REV from frozen RHOC multisig wallets into Co-op Escrow
  // https://github.com/rchain/rchain/commit/1c4f83896a09d06a08fcbc19e3534bf7debe84c9
  val multiSigRhocCase: Seq[Transaction] = List(
    (
      "1111yyTrDHwp1E7eCvnXioqHo5XYPSRy9Sa39FRQuQb6zknsbKQMJ",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      113911827L
    ),
    (
      "111123DVqkXrwFrVMhcZeEKQYZtyXUDX4apZoU5e7bKDnFxo86MMzH",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      6000L
    ),
    (
      "11112cb7Y2VEn5wQLej8YCe51H2GnUdFzbmTuog3NKrhPjjo4CmaE7",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      278000L
    ),
    (
      "11114yAC6A8UMhej5TcrSEUMWkmhigrjUKRDNw3TAvWoUpyy7eQGe",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      334000000L
    ),
    (
      "1111szfAQYVPR89w6uuiaFBmCuZhSjtJqG79k2LvdAHtuqQLUNuSa",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      709000000000L
    ),
    (
      "1111mVkwH4cNqGFJUVexm547sTDqxGiHT5V7Hgnmbahv2YVJ7Ho12",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      2992750000L
    ),
    (
      "11112FJN27C34UJ74QnQ9SWewSqfULP5mbHmYbiQfHJDJKdVQok58J",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      200000000L
    ),
    (
      "1111JrPrPHQ6ouxFtNB9AGj2U8HJrgx23XqtQRp8KgQ9W9NHrZC4X",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      100000000L
    ),
    (
      "1111Mez7uA98KLUDTUiHX5YrwkZQLvKiKZAyxuBNnTZ1wqBT9taZZ",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      851202141L
    ),
    (
      "111155LWAZmK3z954xXdgGL4NTSr3hfSA6SF3a9s3RMHrnm1kKWmC",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      802059300000L
    ),
    (
      "11112arYYZNJv6fEx4kkzEitrQS951xnuY311BRWjkhe3wqR68UVot",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      199214L
    ),
    (
      "11112CWQn4iB5wYhLZfcJfTNB1KRkDdKX3qRdvwYEW243QMLyCwQtV",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      200001L
    ),
    (
      "11112JjWJttFn7jTvHiiuuGbE1FHWPabP6KFHo1RjxCoxSBBH8Edhp",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      23055366800000L
    ),
    (
      "1111gnXjhGZM9F1XTQbwAM8XsgTQq2jCAa6Bu6mUWbjrue8ou6uzM",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      666833L
    ),
    (
      "1111ZfHnsAN6WCAdhgmd9KqxZQq7aSzs9FXYUdkYpsr5EFPdxb5sT",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      600000L
    ),
    (
      "111128uSxdK5XQcPy9o4XWZgoC8SheiBLEiwbXRa8AsWKK3hMXiLbn",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      1L
    ),
    (
      "1111vFwkceSEPiDk6t9oC1ryFAPpGc8zAY83WMmPN9MXk9FYAcrHK",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      425736445456L
    ),
    (
      "1111CKhxcZBYpdm5P1iqJBsZdYPwHjnN8o4PPAW86c6qwyZWw81Z5",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      900000L
    ),
    (
      "1111PEw4wKQLaWBpwVdQbnv5KYE4hXAogXeLySTxHDxRuaPzuZNG1",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      920000L
    ),
    (
      "111127JbiyW4eMZJCr6QebBgkgvk97DGeygecHBcJd9mMiZ5EF3YnG",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      400000000000L
    ),
    (
      "1111PnyhUaDdWLRaBwAJCCTwuYhQ2CgRduzBNUKvhKzyNHi6BqYYt",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      1500000000L
    ),
    (
      "11112bo93nUSFbwPyBac76nNMjvUZku6Sn3PZfg45X39JRH1TdHc2E",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      500000000000L
    ),
    (
      "1111Cb5gCywvPZRcaRyNJwTmjLBqmuBcd69yiFD7kWGBGTFuWQhsc",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      123888875532825L
    ),
    (
      "11114n2ABnLj4MPg6p7K21pLTo2Ba7GwNiHQoFSWbMehAjXQDuXSh",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      28089142976L
    ),
    (
      "11112sFQkSPhNRW89UKvXdDkpjy2oavHAwYQbM4ZjT5gJkKCv1AVBV",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      10000000000L
    ),
    (
      "1111fSRL2QtE6u4XW7631oEYCzrm5FC1RmXDKn1myC5vA1PYXQv4x",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      97442948585L
    ),
    (
      "1111EuHdop7WXbc5YmJ32W3346pJQ5iaJ34TCQPotS5UCB6FZeWAk",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      237857L
    ),
    (
      "11112D4SBuovnUTvceojeGut1BWPESBrnNviUADZXHziL9mhQmJCMK",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      33542049164080L
    ),
    (
      "11112BLVsPRTAJkb5sBQna9hR2q5isnjitqp48UqAcDJoN6agQoC6e",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      492105856L
    ),
    (
      "11112wPvMS7t9uVy52BtVPFt9a1ScVhrcHLHThwNuKbJnqiZR7VgMx",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      6076000L
    ),
    (
      "111129rRaQtqHdtkmryMVVhsQNXjSPdbNYyhvyuo8uqtsEMBRXT4fg",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      1190622L
    ),
    (
      "1111266XWAw5tsFLYFMaKvJx8NFxG8fCM8KjQpJCPCFZzETP1u1RoL",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      200000000000L
    ),
    (
      "11117WyybqVatycVNWkKw1ByoDUqT4784ShDgpgJdJA1cEvjA9XvT",
      "11112YnRy3SPFwJHkTeagQNrruEUTBUxQV7YPm5zomFGKiK1Z62VHg",
      89787L
    )
  ).map(toTransaction(_, ethMultiSigRhocCustomMes))

  val allSpecialCases: Seq[Transaction] = perValidatorCases ++ coopCase ++ multiSigRhocCase
}
