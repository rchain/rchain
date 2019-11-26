CREATE TABLE deploys
(
    signature                BLOB PRIMARY KEY NOT NULL,
    public_key               BLOB             NOT NULL,
    term                     VARCHAR          NOT NULL,
    timestamp                INTEGER          NOT NULL,
    phlo_price               INTEGER          NOT NULL,
    phlo_limit               INTEGER          NOT NULL,
    valid_after_block_number INTEGER          NOT NULL
);