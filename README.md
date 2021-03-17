# Icing

I _sync_ your code to your students and vice versa.

Written in acceptable-quality _Haskell_ and horrible-quality _JavaScript_.

PRs welcome!

_No refunds._

## Screenshot

![screenshot showing the editor in action](https://user-images.githubusercontent.com/11269173/111480859-b46ba480-8732-11eb-86ca-17ad4fa08870.png)

## Installing

If you use the [Nix](https://nixos.org/) package manager, use the following commands:
```sh
nix-build -A icing-server -f server/default.nix # builds the server
nix-build -A icing-client -f client/default.nix # builds the client
```

## Developing

If you use Nix, there's a fully functioning shell provided in `client/` and `server/`.
Just call `nix-shell shell.nix` in the right folder!

### Client

You can also use Yarn -- `yarn start` watches the directory for changes, `yarn build` builds the project.

### Server

You can also use Cabal. If you're reading this, you probably know how to operate Cabal.

## Design

* Uses [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API) with JSON contents to quickly transfer changes between the server and its users.
* Uses operational transformation theory[1] to achieve conflict resolution.

### References
* [1]: SUN, Chengzheng, et al. Achieving convergence, causality preservation, and intention preservation in real-time cooperative editing systems. _ACM Transactions on Computer-Human Interaction (TOCHI)_, 1998, 5.1: 63-108. Available from: https://www.cs.cityu.edu.hk/~jia/research/reduce98.pdf
