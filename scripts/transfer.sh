#!/usr/bin/env bash
set -ex

tmpfile=`mktemp /tmp/transfer.rho.XXXXXXXXXX`
pk=`cat $1`

cat >>$tmpfile << END
new
  rl(`rho:registry:lookup`), RevVaultCh,
in {

  rl!(`rho:id:1o93uitkrjfubh43jt19owanuezhntag5wh74c6ur5feuotpi73q8z`, *RevVaultCh) |
  for (@(_, RevVault) <- RevVaultCh) {

    // REPLACE THE REV ADDRESSES HERE vvv
    match (
      "%FROM",
      "%TO",
      %AMOUNT
    ) {
      (from, to, amount) => {

        new vaultCh, revVaultkeyCh in {
          @RevVault!("findOrCreate", from, *vaultCh) |
          @RevVault!("deployerAuthKey", *revVaultkeyCh) |
          for (@(true, vault) <- vaultCh; key <- revVaultkeyCh) {

            log!(("Beginning transfer of ", amount, "REV from", from, "to", to)) |

            new resultCh in {
              @vault!("transfer", to, amount, *key, *resultCh) |
              for (@result <- resultCh) {

                log!(("Finished transfer of ", amount, "REV to", to, "result was:", result))
              }
            }
          }
        }
      }
    }
  } |

  contract log(@data) = {
    @"DEMO"!(data) | stdout!(data)
  }
}

END

sed -e '' $3 $2 > $2.run

rnode deploy --phlo-limit 10000000000000 --phlo-price 1 --private-key $pk $tmpfile
rnode propose

rm $tmpfile
