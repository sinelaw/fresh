from invoicing import store


def test_empty_store(tmp_path, monkeypatch):
    monkeypatch.setattr(store, "STORE", tmp_path / "missing")
    assert store.load_invoices() == []
