Instantiate
```bash
sudo su -

wget https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz

mkdir ffmpeg-release-amd64-static

tar -xf ffmpeg-release-amd64-static.tar.xz --strip-components=1 -C ffmpeg-release-amd64-static

rm -f ffmpeg-release-amd64-static.tar.xz

ln -s /root/ffmpeg-release-amd64-static/ffmpeg /usr/local/bin/ffmpeg
ln -s /root/ffmpeg-release-amd64-static/ffprobe /usr/local/bin/ffprobe


python -m ensurepip --upgrade
python -m pip install --upgrade pip


sudo yum -y install git

git clone https://github.com/juanmcasillas/gopro2gpx
cd gopro2gpx/
python3 setup.py install
cd ..

curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install

sudo amazon-linux-extras install java-openjdk11 -y
```

generate gpx and duration
```bash
origin="prismaris-roads/JQBR/2022-10-31/tramo 7-8-9"

for path in $(aws s3 ls "$origin/");
do
if [[ "$path" == *.MP4 ]]; then
videoName=${path%.MP4}
echo $videoName
aws s3 cp "s3://$origin/$videoName.MP4" .
python3 -m gopro2gpx -s $videoName.MP4 $videoName  
ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 $videoName.MP4 > $videoName.duration
aws s3 cp $videoName.gpx "s3://$origin/GPX/$videoName.gpx"
aws s3 cp $videoName.duration "s3://$origin/GPX/$videoName.duration"
rm -f $videoName.MP4
fi

done
```



Generate input from gpx (already downloaded)
```bash
touch inputSTR.txt
for videoNameGPX in *.gpx; do
videoName=${videoNameGPX%.gpx}    
echo "$videoName.duration;$videoNameGPX;$videoName.srt;Tramo 5" >> inputSTR.txt
done
```

Execution to calc SRT (need fix by eje)
```bash
aws s3 cp s3://prismaris-roads/conviasdesktop-assembly-1.13.jar .
aws s3 cp s3://prismaris-roads/tramo2.xml .


java -jar conviasdesktop-assembly-1.13.jar tramo2.xml tramo2.xml < inputSTR.txt
```

Upload SRT
```bash
origin="prismaris-roads/JQBR/2022-10-31/tramo 4sg/SRT/"
for videoNameSRT in *.srt; do
videoName=${videoNameSRT%.srt}
aws s3 cp $videoNameSRT "s3://$origin$videoNameSRT"
done
```

Generation of output finale
```bash
ffmpeg -i input.MP4 -i logo.png -filter_complex "[0]subtitles=input.srt:force_style='Alignment=1,OutlineColour=&H1000000&,PrimaryColour=&H000FFFF&,BackColour=&HFF&,BorderStyle=1,Outline=1,Shadow=0,Fontsize=14'[bg];[bg][1]overlay=0:0" -crf 18 -t 5 "output.mp4"
aws s3 cp salida.mp4 s3://prismaris-roads/JQBR/2022-09-30/Tramo3/output/GH030653.mp4
```

```bash
sudo su -

wget https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz

mkdir ffmpeg-release-amd64-static

tar -xf ffmpeg-release-amd64-static.tar.xz --strip-components=1 -C ffmpeg-release-amd64-static

rm -f ffmpeg-release-amd64-static.tar.xz

ln -s /root/ffmpeg-release-amd64-static/ffmpeg /usr/local/bin/ffmpeg
ln -s /root/ffmpeg-release-amd64-static/ffprobe /usr/local/bin/ffprobe

curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install


```
```bash
aws s3 cp "s3://prismaris-roads/JQBR/2022-10-31/run_22.sh" .
chmod +x run_22.sh
./run_22.sh

```

load SRT and show progresivas
```bash
aws s3 cp "s3://prismaris-roads/JQBR/2022-10-31/tramo 5-6/06/SRT" Tramo5 --recursive

for videoNameSRT in *.srt; do 
  echo $videoNameSRT
  head -n 5 $videoNameSRT | tail -n 1
done
```