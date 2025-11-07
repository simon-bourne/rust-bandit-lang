use futures::{FutureExt, channel::oneshot, future};

use crate::SharedMut;

#[derive(Clone)]
pub struct Latch(SharedMut<LatchData>);

impl Latch {
    pub fn closed() -> Self {
        let (send, receive) = oneshot::channel();

        Self(SharedMut::new(LatchData {
            send: Some(send),
            receive: receive.shared(),
        }))
    }

    pub fn open(&self) {
        if let Some(send) = self.0.borrow_mut().send.take() {
            send.send(()).expect("Expected open channel");
        }
    }

    pub async fn wait(&self) {
        let receive = self.0.borrow().receive.clone();

        receive.await.expect("Expected open channel")
    }
}

struct LatchData {
    send: Option<oneshot::Sender<()>>,
    receive: future::Shared<oneshot::Receiver<()>>,
}
