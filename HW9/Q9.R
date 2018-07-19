library(EBImage)
library(jpeg)
pic = flip(readImage("stock.jpg"))

pic.r = pic[,,1]
pic.g = pic[,,2]
pic.b = pic[,,3]

pic.r.pca = prcomp(pic.r, center = F)
pic.g.pca = prcomp(pic.g, center = F)
pic.b.pca = prcomp(pic.b, center = F)

pca = list(pic.r.pca, pic.g.pca, pic.b.pca)

nValues = seq(3, ncol(pic.r), by = 3)
compressed_img = list()
for(n in nValues) {
  print(n)
  compressed_img[[n]] = sapply(pca, function(p) {
    p$x[ ,1:n] %*% t(p$rotation[,1:n])
  }, simplify = 'array')
  writeJPEG(rotate(compressed_img[[n]], angle = 90),
            paste('compressed/', n, '_components.jpg', sep = ''),
            quality = 1)
}


calc_size = function(n) {
  data_size = nrow(pic.r) * n
  rotation_size = ncol(pic.r) * n
  (data_size + rotation_size) / 1000 * 3
}
originalSize = nrow(pic.r) * ncol(pic.r) * 3 / 1000
compressed_info = data.frame(components = nValues, size = calc_size(nValues))
intersection.point = round(originalSize / (compressed_info[1,]$size / compressed_info[1,]$components))
ggplot(compressed_info, aes(x = components, y = size, color = 1)) +
  geom_line() +
  geom_hline(yintercept = originalSize, color = 'red') +
  geom_vline(xintercept = intersection.point, color = 'red', linetype = 2) +
  geom_text(data = data.frame(x = 40, y = originalSize), aes(x, y + 50),
            label = 'Original Size',
            color = 'red') +
  geom_text(data = data.frame(x = intersection.point, y = 60), aes(x + 15, y),
            label = intersection.point,
            color = 'red') +
  guides(color = F) +
  labs(y = 'Size(KB)')

library(animation)
library(imager)

saveGIF({
  for (n in nValues) {
    print(n)
    path <- paste('compressed/', n, '_components.jpg', sep='')
    image <- load.image(path)
    plot(image, main = paste('Number of components =', n), axes = F)
  }
  },
  interval = 0.2
)
