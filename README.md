# acl2ml
Mirror and Docker build for ACL2(ml) by Jonathan Heras

Home page of ACL2(ml) is at https://www.macs.hw.ac.uk/~ek19/ACL2ml/ .

This repo is cloned from the zip files for ACL2(ml) LPARS v1 at https://www.macs.hw.ac.uk/~ek19/ACL2ml/acl2ml.zip and ACL2(ml) v2 at https://www.macs.hw.ac.uk/~ek19/ACL2ml/acl2ml_v2.zip .

Basic build and run:
```bash
git clone https://github.com/jimwhite/acl2ml.git
cd acl2ml
docker buildx build -t acl2ml .
docker run -it acl2ml bash
```

Instructions are in [acl2ml_manual.pdf](./manual/acl2ml_manual.pdf)
