The pre-0.2 implementation, kept for reference and for A/B comparison.

It is not sourced or loaded by the package. To reproduce its ingestion and
chunking behaviour under the current code, use the `legacy_v1` recipe:

    answer_document(file, question, recipe = "legacy_v1")
