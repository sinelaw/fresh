.class public Lcom/example/HelloActivity;
.super Landroid/app/Activity;
.source "HelloActivity.java"

.method public onCreate(Landroid/os/Bundle;)V
    .locals 2

    const-string v0, "Hello from Smali"
    invoke-static {v0}, Landroid/util/Log;->d(Ljava/lang/String;)I

    if-eqz p1, :done
    return-void

:done
    new-instance v1, Ljava/lang/Object;
    return-void
.end method
