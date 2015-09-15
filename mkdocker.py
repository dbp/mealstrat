import os

proj = os.path.basename(os.getcwd())

os.system("mkdir -p scratch")

os.system("rm -rf scratch/root")

libs = [x.split()[2] for x in os.popen("ldd %s" % (proj)).readlines() if "=>" in x and "/" in x]

extra = ["/bin/sh",
         "/lib64/ld-linux-x86-64.so.2",
         "/usr/lib/x86_64-linux-gnu/gconv/UTF-16.so",
         "/usr/lib/x86_64-linux-gnu/gconv/UTF-32.so",
         "/usr/lib/x86_64-linux-gnu/gconv/UTF-7.so",
         "/usr/lib/x86_64-linux-gnu/gconv/gconv-modules",
         "/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache",
         "/lib/x86_64-linux-gnu/libnss_files.so.2",
         "/lib/x86_64-linux-gnu/libnss_dns.so.2",
         "/lib/x86_64-linux-gnu/libresolv.so.2",
         "/etc/protocols",
         "/etc/services",
         "/etc/ssl/certs",
         "/usr/share/ca-certificates"
         ]

for x in libs+extra:
    os.system("mkdir -p scratch/root" + os.path.dirname(x))
    os.system("cp -L " + x + " scratch/root" + x)

os.system("mkdir -p scratch/root/tmp")

os.system("cp -R /etc/ssl/certs scratch/root/etc/ssl/certs")
os.system("cp -R /usr/share/ca-certificates scratch/root/usr/share/ca-certificates")

os.system("tar -cC scratch/root .| docker import - haskell-scratch:latest")

os.system("rm -rf scratch")
