package com.revdefine.node.store

import cats.effect.Resource

import java.nio.ByteBuffer

trait DefineKeyValueStore[F[_]] {
  def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]]

  def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit]

  def delete(keys: Seq[ByteBuffer]): F[Int]

  def iterate: Resource[F, Iterator[(ByteBuffer, ByteBuffer)]]
}
